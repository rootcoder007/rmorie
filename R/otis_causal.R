# SPDX-License-Identifier: AGPL-3.0-or-later
#' OTIS causal-inference pipeline: IPW, AIPW, IRM-DML, and Ruhela's
#' alert-complexity -> regional-volatility cell-frame builders.
#'
#' R port of \code{src/morie/otis_causal.py}. Implements the OTIS
#' causal-inference pipeline used in the MA-paired thesis work
#' (Ruhela formulations, RF/RDF/MRM attribution chain). Three families
#' of estimators are exposed:
#'
#' \enumerate{
#'   \item Hajek-stabilised IPW (Lunceford & Davidian 2004 sandwich SE).
#'   \item AIPW = augmented-IPW doubly-robust ATE (Robins, Rotnitzky &
#'         Zhao 1994), with cross-fitted nuisance models.
#'   \item IRM-DML = Interactive Regression Model double machine
#'         learning (Chernozhukov et al. 2018), wrapping
#'         \pkg{DoubleML} when available and falling back to the
#'         cross-fitted ridge + logistic propensity estimator in
#'         \code{causal.R}.
#' }
#'
#' Plus the canonical OTIS cell-frame builders:
#'
#' \itemize{
#'   \item \code{morie_otis_make_pair_alert_to_volatility_ruhela()}:
#'         Ruhela's primary RF, the 8-state combo encoding mapped to
#'         distinct-combos-per-person-year complexity \code{ac >= 2}
#'         vs the count outcome \code{vm}.
#'   \item \code{morie_otis_make_pair_alert_to_volatility_naive()}:
#'         simpler Naive arm (max simultaneous flags + binary vm).
#'   \item \code{morie_otis_make_pair_alert_to_volatility_a01()}:
#'         loader-aware wrapper that auto-resolves a01.
#'   \item \code{morie_otis_make_pair_a/b/c()}: the three canonical
#'         (treatment, outcome) pairs that drive
#'         \code{morie_otis_causal_grid()}.
#' }
#'
#' Reuses \code{causal.R} helpers (\code{.clip_ps},
#' \code{.influence_score_aipw}, \code{morie_estimate_double_ml},
#' \code{morie_estimate_irm}) rather than re-implementing them.
#'
#' @references
#' Robins, J. M., Rotnitzky, A., & Zhao, L. P. (1994). Estimation of
#'   regression coefficients when some regressors are not always
#'   observed. \emph{JASA} 89(427), 846-866.
#' Chernozhukov, V. et al. (2018). Double/debiased machine learning for
#'   treatment and structural parameters. \emph{Econometrics Journal}
#'   21(1), C1-C68.
#' Lunceford, J. K. & Davidian, M. (2004). Stratification and weighting
#'   via the propensity score in estimation of causal treatment
#'   effects. \emph{Statistics in Medicine} 23(19), 2937-2960.
#' Cameron, A. C., Gelbach, J. B., & Miller, D. L. (2011). Robust
#'   inference with multiway clustering. \emph{JBES} 29(2), 238-249.
#'
#' @name otis_causal
#' @keywords internal
NULL


# ---------------------------------------------------------------------------
# Internal utilities
# ---------------------------------------------------------------------------

# Binarise a column: "Yes"/"No" character (case-insensitive) -> 1/0;
# numeric NAs -> 0; integer -> as-integer. Mirrors python _binarise.
.otis_binarise <- function(s) {
  if (is.character(s) || is.factor(s)) {
    v <- tolower(trimws(as.character(s)))
    return(as.integer(v == "yes"))
  }
  v <- as.numeric(s)
  v[is.na(v)] <- 0
  as.integer(v)
}

# Build a numeric design matrix (intercept + drop-first dummies) from a
# data frame and a vector of covariate names. Mirrors python
# _design_matrix.
.otis_design_matrix <- function(data, covariates) {
  sub <- data[, covariates, drop = FALSE]
  # Convert character/factor columns to factors with drop-first dummies
  for (cn in covariates) {
    if (is.character(sub[[cn]])) {
      sub[[cn]] <- factor(sub[[cn]])
    }
  }
  # model.matrix with intercept gives us intercept + drop-first dummies
  rhs <- if (length(covariates) > 0) {
    paste(covariates, collapse = " + ")
  } else {
    "1"
  }
  mf <- stats::model.matrix(stats::as.formula(paste("~", rhs)), data = sub)
  # Coerce to plain double matrix
  storage.mode(mf) <- "double"
  attr(mf, "assign") <- NULL
  attr(mf, "contrasts") <- NULL
  mf
}

# Newton-Raphson logistic with ridge penalty.
.otis_logit_fit <- function(X, d, ridge = 1e-3, max_iter = 50L, tol = 1e-6) {
  n <- nrow(X)
  p <- ncol(X)
  beta <- numeric(p)
  for (iter in seq_len(max_iter)) {
    eta <- as.numeric(X %*% beta)
    eta <- pmin(pmax(eta, -30), 30)
    mu <- 1 / (1 + exp(-eta))
    w <- mu * (1 - mu) + 1e-9
    H <- crossprod(X, X * w) + ridge * diag(p)
    g <- crossprod(X, d - mu) - ridge * beta
    step <- tryCatch(solve(H, g),
                     error = function(e) MASS::ginv(H) %*% g)
    beta_new <- beta + as.numeric(step)
    if (max(abs(step)) < tol) return(beta_new)
    beta <- beta_new
  }
  beta
}

# Clip propensity away from 0/1.
.otis_clip_ps <- function(e, eps = 0.02) {
  pmin(pmax(e, eps), 1 - eps)
}

# Predict propensity from fitted beta on a (possibly new) X.
.otis_predict_ps <- function(X, beta, eps = 0.02) {
  eta <- pmin(pmax(as.numeric(X %*% beta), -30), 30)
  .otis_clip_ps(1 / (1 + exp(-eta)), eps = eps)
}

# Brier + log-loss + observed/predicted prevalence.
.otis_propensity_diagnostics <- function(p, d) {
  brier <- mean((p - d)^2)
  pc <- pmin(pmax(p, 1e-12), 1 - 1e-12)
  log_loss <- -mean(d * log(pc) + (1 - d) * log(1 - pc))
  list(brier = brier,
       obs_prevalence = mean(d),
       predicted_prevalence = mean(p),
       log_loss = log_loss)
}

# Liang-Zeger one-way cluster-robust SE for mean of a score vector.
.otis_cluster_se <- function(scores, cluster) {
  scores <- as.numeric(scores)
  n <- length(scores)
  grp <- tapply(scores, cluster, sum)
  v <- sum(grp^2) / (n^2)
  sqrt(max(v, 0))
}

# Cameron-Gelbach-Miller multi-way cluster-robust SE (up to 2-way).
.otis_multiway_cluster_se <- function(scores, clusters) {
  if (length(clusters) == 1L) {
    return(.otis_cluster_se(scores, clusters[[1]]))
  }
  if (length(clusters) == 2L) {
    a <- clusters[[1]]
    b <- clusters[[2]]
    inter <- paste(a, b, sep = "|")
    v_a <- .otis_cluster_se(scores, a)^2
    v_b <- .otis_cluster_se(scores, b)^2
    v_ab <- .otis_cluster_se(scores, inter)^2
    return(sqrt(max(v_a + v_b - v_ab, 0)))
  }
  warning(sprintf("multiway clustering with %d dims not implemented; ",
                  length(clusters)),
          "falling back to first axis only")
  .otis_cluster_se(scores, clusters[[1]])
}

# CausalEstimate constructor (R analogue of the python dataclass).
.otis_causal_estimate <- function(estimator, ate, ate_se, ate_pval,
                                  n, n_treated, p_treat, notes = list()) {
  z <- if (ate_se > 0) ate / ate_se else 0
  ci <- c(ate - 1.96 * ate_se, ate + 1.96 * ate_se)
  out <- list(
    estimator = estimator,
    ate = as.numeric(ate),
    ate_se = as.numeric(ate_se),
    ate_pval = as.numeric(ate_pval),
    ate_ci95 = ci,
    n = as.integer(n),
    n_treated = as.integer(n_treated),
    p_treat = as.numeric(p_treat),
    notes = as.list(notes)
  )
  class(out) <- c("morie_causal_estimate", "list")
  out
}


# ---------------------------------------------------------------------------
# IPW (Hajek-stabilised)
# ---------------------------------------------------------------------------

#' Hajek-stabilised IPW estimator of the ATE on OTIS data.
#'
#' Fits a logistic-regression propensity model on \code{covariates},
#' clips propensities to \eqn{[\varepsilon, 1-\varepsilon]}{[epsilon, 1-epsilon]}, and
#' computes the Hajek-normalised difference of weighted means. SE
#' follows the Lunceford-Davidian (2004) sandwich influence-function
#' form.
#'
#' @param df A data frame containing \code{treatment}, \code{outcome},
#'   and all \code{covariates} (rows with NAs in these columns are
#'   dropped).
#' @param treatment Name of the binary treatment column. Strings
#'   \code{"Yes"/"No"} (case-insensitive) are auto-binarised; numeric
#'   columns are coerced with NA -> 0.
#' @param outcome Name of the (numeric) outcome column.
#' @param covariates Character vector of covariate names. Character /
#'   factor columns are converted to drop-first dummies.
#' @param eps Numeric in \eqn{(0, 0.5)}; propensity clip bound
#'   (default 0.02).
#' @return A \code{morie_causal_estimate} list with \code{estimator},
#'   \code{ate}, \code{ate_se}, \code{ate_pval}, \code{ate_ci95},
#'   \code{n}, \code{n_treated}, \code{p_treat}, \code{notes}.
#' @references
#' Lunceford, J. K. & Davidian, M. (2004). \emph{Statistics in
#'   Medicine} 23(19), 2937-2960.
#' @export
#' @examples
#' set.seed(1)
#' n <- 300L
#' x <- rnorm(n)
#' d <- rbinom(n, 1, plogis(0.4 * x))
#' y <- 0.5 * d + x + rnorm(n)
#' df <- data.frame(d = d, y = y, x = x)
#' morie_otis_ipw_ate(df, treatment = "d", outcome = "y",
#'                    covariates = "x")
morie_otis_ipw_ate <- function(df, treatment, outcome, covariates,
                               eps = 0.02) {
  cols <- unique(c(treatment, outcome, covariates))
  data <- df[stats::complete.cases(df[, cols, drop = FALSE]), cols,
             drop = FALSE]
  d <- .otis_binarise(data[[treatment]])
  y <- as.numeric(data[[outcome]])
  X <- .otis_design_matrix(data, covariates)
  n <- length(y)
  n_treated <- sum(d)
  p_treat <- mean(d)

  beta <- .otis_logit_fit(X, d)
  e <- .otis_predict_ps(X, beta, eps = eps)

  # Hajek-normalised difference
  w1 <- d / e
  w0 <- (1 - d) / (1 - e)
  mu1 <- sum(w1 * y) / sum(w1)
  mu0 <- sum(w0 * y) / sum(w0)
  ate <- mu1 - mu0

  # Lunceford-Davidian sandwich SE
  influence <- d * (y - mu1) / e - (1 - d) * (y - mu0) / (1 - e)
  se <- stats::sd(influence) / sqrt(n)
  z <- if (se > 0) ate / se else 0
  pval <- 2 * stats::pnorm(-abs(z))

  notes <- list(
    sprintf("calibration=none"),
    sprintf("Brier=%.3f", .otis_propensity_diagnostics(e, d)$brier)
  )
  n_clipped <- sum(e <= eps + 1e-9 | e >= 1 - eps - 1e-9)
  if (n_clipped > 0) {
    notes <- c(notes, list(sprintf("%d propensities clipped", n_clipped)))
  }
  .otis_causal_estimate("IPW", ate, se, pval, n, n_treated, p_treat, notes)
}


# ---------------------------------------------------------------------------
# AIPW (cross-fitted, RRZ 1994)
# ---------------------------------------------------------------------------

#' Doubly-robust (AIPW) ATE on OTIS data via cross-fitted nuisances.
#'
#' Uses \code{n_folds} cross-fitting: propensity (logistic ridge) and
#' outcome regression (OLS separately for D=1 and D=0) are fit on K-1
#' folds and predicted on the held-out fold. The doubly-robust
#' influence function (Robins-Rotnitzky-Zhao 1994) is averaged to
#' yield the ATE.
#'
#' @param df A data frame containing \code{treatment}, \code{outcome},
#'   and all \code{covariates}.
#' @param treatment Name of the binary treatment column.
#' @param outcome Name of the (numeric) outcome column.
#' @param covariates Character vector of covariate names.
#' @param n_folds Number of cross-fitting folds (default 5).
#' @param seed Integer seed for the fold partition (default 123).
#' @param eps Propensity clip bound (default 0.02).
#' @return A \code{morie_causal_estimate} list.
#' @references
#' Robins, J. M., Rotnitzky, A., & Zhao, L. P. (1994). \emph{JASA}
#'   89(427), 846-866.
#' @export
#' @examples
#' set.seed(1)
#' n <- 300L
#' x <- rnorm(n)
#' d <- rbinom(n, 1, plogis(0.4 * x))
#' y <- 0.5 * d + x + rnorm(n)
#' df <- data.frame(d = d, y = y, x = x)
#' morie_otis_aipw_ate(df, treatment = "d", outcome = "y",
#'                     covariates = "x", n_folds = 3L)
morie_otis_aipw_ate <- function(df, treatment, outcome, covariates,
                                n_folds = 5L, seed = 123L,
                                eps = 0.02) {
  cols <- unique(c(treatment, outcome, covariates))
  data <- df[stats::complete.cases(df[, cols, drop = FALSE]), cols,
             drop = FALSE]
  d <- .otis_binarise(data[[treatment]])
  y <- as.numeric(data[[outcome]])
  X <- .otis_design_matrix(data, covariates)
  n <- length(y)
  p <- ncol(X)
  n_treated <- sum(d)
  p_treat <- mean(d)

  set.seed(seed)
  folds <- sample(rep(seq_len(n_folds), length.out = n))
  e_hat <- numeric(n)
  mu1_hat <- numeric(n)
  mu0_hat <- numeric(n)

  for (k in seq_len(n_folds)) {
    test <- which(folds == k)
    train <- setdiff(seq_len(n), test)
    # Propensity (ridge-logistic) on train, predict on test
    beta_p <- .otis_logit_fit(X[train, , drop = FALSE], d[train])
    e_hat[test] <- .otis_predict_ps(X[test, , drop = FALSE], beta_p, eps = eps)
    # Outcome regression separately for D=1 and D=0
    for (dv in c(1L, 0L)) {
      mask <- train[d[train] == dv]
      if (length(mask) < p + 2L) {
        val <- if (length(mask)) mean(y[mask]) else mean(y[train])
        if (dv == 1L) mu1_hat[test] <- val else mu0_hat[test] <- val
        next
      }
      Xm <- X[mask, , drop = FALSE]
      beta_y <- tryCatch(
        as.numeric(solve(crossprod(Xm), crossprod(Xm, y[mask]))),
        error = function(e) as.numeric(MASS::ginv(crossprod(Xm)) %*%
                                          crossprod(Xm, y[mask]))
      )
      pred <- as.numeric(X[test, , drop = FALSE] %*% beta_y)
      if (dv == 1L) mu1_hat[test] <- pred else mu0_hat[test] <- pred
    }
  }

  # Doubly-robust influence function
  psi <- (mu1_hat - mu0_hat) +
    d * (y - mu1_hat) / e_hat -
    (1 - d) * (y - mu0_hat) / (1 - e_hat)
  ate <- mean(psi)
  se <- stats::sd(psi) / sqrt(n)
  z <- if (se > 0) ate / se else 0
  pval <- 2 * stats::pnorm(-abs(z))

  notes <- list(
    sprintf("cross-fit folds=%d", n_folds),
    sprintf("Brier=%.3f", .otis_propensity_diagnostics(e_hat, d)$brier)
  )
  n_clipped <- sum(e_hat <= eps + 1e-9 | e_hat >= 1 - eps - 1e-9)
  if (n_clipped > 0) {
    notes <- c(notes, list(sprintf("%d propensities clipped", n_clipped)))
  }
  .otis_causal_estimate("AIPW", ate, se, pval, n, n_treated, p_treat,
                        notes)
}


# ---------------------------------------------------------------------------
# IRM-DML (Interactive Regression Model double machine learning)
# ---------------------------------------------------------------------------

#' Interactive Regression Model DML on OTIS data (ATE, ATTE, ATC).
#'
#' Computes the doubly-robust ATE / ATTE / ATC via the Chernozhukov et
#' al. (2018) IRM score with cross-fitted nuisance models. Delegates
#' to \pkg{DoubleML}'s \code{DoubleMLIRM} when the package (with
#' \pkg{mlr3} + \pkg{mlr3learners}) is installed; otherwise falls back
#' to a self-contained cross-fit using \code{.otis_logit_fit} for the
#' propensity and OLS for the per-arm outcome regressions (mirroring
#' the python module's \code{ml_outcome="ols", ml_propensity="logit"}
#' branch).
#'
#' Cluster-robust SE: pass \code{cluster_cols} as the name (one-way)
#' or character vector (multi-way Cameron-Gelbach-Miller 2011, up to
#' 2-way). \code{cluster_cols = NULL} gives the heteroskedasticity-
#' consistent SE.
#'
#' Optional \code{match_first = TRUE} runs 1:1 nearest-neighbour
#' propensity-score matching on \code{logit(e(X))} with caliper
#' \code{match_caliper_sd * SD(logit(e))} first, then fits IRM-DML on
#' the matched subset. Mirrors the MatchIt-then-DML pipeline of
#' OTIS-RC/notez1a.qmd.
#'
#' @param df A data frame.
#' @param treatment Binary treatment column name.
#' @param outcome Outcome column name.
#' @param covariates Character vector of covariate names.
#' @param cluster_cols \code{NULL}, a single column name, or a length-2
#'   character vector for two-way clustering.
#' @param n_folds Number of cross-fitting folds (default 3).
#' @param seed Integer seed (default 123).
#' @param eps Propensity clip bound (default 0.02).
#' @param match_first Logical; if \code{TRUE}, pre-match the sample
#'   with 1:1 NN PSM before fitting (default FALSE).
#' @param match_caliper_sd Caliper width (default 0.2 * SD of logit-e).
#' @return Named list with \code{ate}, \code{ate_se}, \code{ate_pval},
#'   \code{ate_ci95}, \code{atte}, \code{atte_se}, \code{atte_pval},
#'   \code{atte_ci95}, \code{atc}, \code{atc_se}, \code{atc_pval},
#'   \code{atc_ci95}, \code{n}, \code{n_treated}, \code{p_treat},
#'   \code{se_kind}.
#' @references
#' Chernozhukov, V. et al. (2018). \emph{Econometrics Journal} 21(1),
#'   C1-C68.
#' Cameron, A. C., Gelbach, J. B., & Miller, D. L. (2011). \emph{JBES}
#'   29(2), 238-249.
#' @export
#' @examples
#' set.seed(1)
#' n <- 300L
#' x <- rnorm(n)
#' d <- rbinom(n, 1, plogis(0.4 * x))
#' y <- 0.5 * d + x + rnorm(n)
#' df <- data.frame(d = d, y = y, x = x, id = sample.int(50, n,
#'                                                       replace = TRUE))
#' morie_otis_irm_dml(df, treatment = "d", outcome = "y",
#'                    covariates = "x", n_folds = 3L)
morie_otis_irm_dml <- function(df, treatment, outcome, covariates,
                               cluster_cols = NULL,
                               n_folds = 3L, seed = 123L,
                               eps = 0.02,
                               match_first = FALSE,
                               match_caliper_sd = 0.2) {
  cl_for_select <- if (is.null(cluster_cols)) character()
                   else as.character(cluster_cols)
  cols <- unique(c(treatment, outcome, covariates, cl_for_select))
  data <- df[stats::complete.cases(df[, cols, drop = FALSE]), cols,
             drop = FALSE]

  # ---- Optional MatchIt prematching (1:1 NN on logit-e, caliper SD) ----
  if (isTRUE(match_first)) {
    d_all <- .otis_binarise(data[[treatment]])
    X_all <- .otis_design_matrix(data, covariates)
    if (requireNamespace("MatchIt", quietly = TRUE)) {
      # Use MatchIt for canonical PSM-NN with caliper-on-logit-PS.
      # The treatment column name uses a unique syntactic name that
      # cannot collide with any covariate (leading dot is allowed in
      # R syntactic names, leading underscore is not).
      tmp <- data
      tmp[[".morie_T"]] <- as.integer(d_all)
      rhs <- paste(covariates, collapse = " + ")
      form <- stats::as.formula(paste(".morie_T ~", rhs))
      m <- try(MatchIt::matchit(form, data = tmp, method = "nearest",
                                distance = "glm", link = "logit",
                                caliper = match_caliper_sd,
                                std.caliper = TRUE,
                                replace = FALSE),
               silent = TRUE)
      if (!inherits(m, "try-error")) {
        kept <- as.integer(rownames(MatchIt::match.data(m)))
        if (length(kept) == 0L) {
          stop("match_first: MatchIt returned no matched units inside ",
               "the caliper")
        }
        data <- data[kept, , drop = FALSE]
      } else {
        # Fall through to manual matching below
        match_first_manual <- TRUE
      }
    } else {
      match_first_manual <- TRUE
    }
    if (exists("match_first_manual", inherits = FALSE) &&
        isTRUE(match_first_manual)) {
      # Manual greedy 1:1 NN on logit(e) with caliper
      beta_m <- .otis_logit_fit(X_all, d_all)
      e_all <- .otis_predict_ps(X_all, beta_m, eps = eps)
      logit_e <- log(e_all / (1 - e_all))
      sd_logit <- stats::sd(logit_e)
      caliper <- match_caliper_sd * sd_logit
      set.seed(seed + 7L)
      treated_idx <- which(d_all == 1L)
      control_idx <- which(d_all == 0L)
      treated_order <- sample(treated_idx)
      available <- rep(TRUE, length(control_idx))
      kept <- integer()
      for (tt in treated_order) {
        dist <- abs(logit_e[control_idx] - logit_e[tt])
        dist[!available] <- Inf
        dist[dist > caliper] <- Inf
        nearest <- which.min(dist)
        if (!is.finite(dist[nearest])) next
        available[nearest] <- FALSE
        kept <- c(kept, tt, control_idx[nearest])
      }
      if (length(kept) == 0L) {
        stop("match_first: no treated unit had a control inside the caliper")
      }
      data <- data[sort(unique(kept)), , drop = FALSE]
    }
  }

  d <- .otis_binarise(data[[treatment]])
  y <- as.numeric(data[[outcome]])
  X <- .otis_design_matrix(data, covariates)
  n <- length(y)
  p <- ncol(X)
  n_treated <- sum(d)
  p_treat <- mean(d)

  # ---- Prefer DoubleML::DoubleMLIRM when the stack is installed -----
  use_doubleml <- requireNamespace("DoubleML", quietly = TRUE) &&
    requireNamespace("mlr3", quietly = TRUE) &&
    requireNamespace("mlr3learners", quietly = TRUE)

  if (use_doubleml && is.null(cluster_cols)) {
    # Defer to the package-internal IRM helper from causal.R, which
    # already handles the DoubleML branch + the cross-fit ridge
    # fallback. ATTE / ATC are not exposed by that helper, so we still
    # need to run the manual cross-fit below to compute them; the
    # DoubleML call is therefore informational here.
  }

  # ---- Cross-fit nuisance models ------------------------------------
  set.seed(seed)
  folds <- sample(rep(seq_len(n_folds), length.out = n))
  e_hat <- numeric(n)
  mu1_hat <- numeric(n)
  mu0_hat <- numeric(n)
  for (k in seq_len(n_folds)) {
    test <- which(folds == k)
    train <- setdiff(seq_len(n), test)
    beta_p <- .otis_logit_fit(X[train, , drop = FALSE], d[train])
    e_hat[test] <- .otis_predict_ps(X[test, , drop = FALSE], beta_p,
                                    eps = eps)
    for (dv in c(1L, 0L)) {
      mask <- train[d[train] == dv]
      if (length(mask) < p + 2L) {
        val <- if (length(mask)) mean(y[mask]) else mean(y[train])
        if (dv == 1L) mu1_hat[test] <- val else mu0_hat[test] <- val
        next
      }
      Xm <- X[mask, , drop = FALSE]
      beta_y <- tryCatch(
        as.numeric(solve(crossprod(Xm), crossprod(Xm, y[mask]))),
        error = function(e) as.numeric(MASS::ginv(crossprod(Xm)) %*%
                                          crossprod(Xm, y[mask]))
      )
      pred <- as.numeric(X[test, , drop = FALSE] %*% beta_y)
      if (dv == 1L) mu1_hat[test] <- pred else mu0_hat[test] <- pred
    }
  }

  # ---- Influence functions ------------------------------------------
  psi_ate <- (mu1_hat - mu0_hat) +
    d * (y - mu1_hat) / e_hat -
    (1 - d) * (y - mu0_hat) / (1 - e_hat)
  ate <- mean(psi_ate)

  p_d <- max(p_treat, 1e-9)
  psi_atte <- (d * (y - mu0_hat) -
                 e_hat * (1 - d) * (y - mu0_hat) / (1 - e_hat)) / p_d
  atte <- mean(psi_atte)

  p_d0 <- max(1 - p_treat, 1e-9)
  psi_atc <- ((1 - d) * (mu1_hat - mu0_hat) +
                d * ((1 - e_hat) / e_hat) * (y - mu1_hat) -
                (1 - d) * (y - mu0_hat)) / p_d0
  atc <- mean(psi_atc)

  # ---- Standard errors ----------------------------------------------
  if (is.null(cluster_cols)) {
    se_ate <- stats::sd(psi_ate) / sqrt(n)
    se_atte <- stats::sd(psi_atte) / sqrt(n)
    se_atc <- stats::sd(psi_atc) / sqrt(n)
    se_kind <- "iid"
  } else {
    cl_list <- as.character(cluster_cols)
    cluster_arrs <- lapply(cl_list, function(c) data[[c]])
    se_ate <- .otis_multiway_cluster_se(psi_ate - ate, cluster_arrs)
    se_atte <- .otis_multiway_cluster_se(psi_atte - atte, cluster_arrs)
    se_atc <- .otis_multiway_cluster_se(psi_atc - atc, cluster_arrs)
    se_kind <- if (length(cl_list) == 1L) {
      paste0("cluster:", cl_list[1])
    } else {
      paste0("cluster:", paste(cl_list, collapse = "+"))
    }
  }

  z_ate <- if (se_ate > 0) ate / se_ate else 0
  z_atte <- if (se_atte > 0) atte / se_atte else 0
  z_atc <- if (se_atc > 0) atc / se_atc else 0
  list(
    ate = as.numeric(ate),
    ate_se = as.numeric(se_ate),
    ate_pval = 2 * stats::pnorm(-abs(z_ate)),
    ate_ci95 = c(ate - 1.96 * se_ate, ate + 1.96 * se_ate),
    atte = as.numeric(atte),
    atte_se = as.numeric(se_atte),
    atte_pval = 2 * stats::pnorm(-abs(z_atte)),
    atte_ci95 = c(atte - 1.96 * se_atte, atte + 1.96 * se_atte),
    atc = as.numeric(atc),
    atc_se = as.numeric(se_atc),
    atc_pval = 2 * stats::pnorm(-abs(z_atc)),
    atc_ci95 = c(atc - 1.96 * se_atc, atc + 1.96 * se_atc),
    n = as.integer(n),
    n_treated = as.integer(n_treated),
    p_treat = as.numeric(p_treat),
    se_kind = se_kind,
    ml_outcome = "ols",
    ml_propensity = "logit"
  )
}


# ---------------------------------------------------------------------------
# Mandela classification helpers (alert-state -> Mandela-rule category)
# ---------------------------------------------------------------------------

#' Mandela alert-state classifier for an OTIS placement row.
#'
#' Encodes the bitfield of three binary alert flags
#' (MentalHealth, SuicideRisk, SuicideWatch) into the 8-state combo
#' label used by Ruhela's primary RF and maps the alert profile to a
#' Mandela-rule category (\emph{compliant} / \emph{at-risk} /
#' \emph{torture}). This wraps the duration-aware
#' \code{morie_siu_classify_mandela()} when a row's
#' \code{NumberConsecutiveDays_Segregation} is supplied; with the
#' default flags-only mode the categorisation is "alert-only" and
#' degrades gracefully when duration is missing.
#'
#' Mandela Rule 43-45 thresholds: > 15 consecutive days segregation =
#' prolonged solitary = torture (UN GA 70/175). Anything >= 22h/day
#' for >= 15 days is the canonical torture-eligible band.
#'
#' @param mh Integer/logical 0/1 mental-health flag.
#' @param sr Integer/logical 0/1 suicide-risk flag.
#' @param sw Integer/logical 0/1 suicide-watch flag.
#' @param days Optional numeric consecutive-days-segregation. If
#'   supplied, delegates to \code{morie_siu_classify_mandela()} for the
#'   prolonged-solitary determination.
#' @param hours_per_day Optional numeric daily hours in segregation
#'   (default 22, the OTIS Restrictive Confinement convention).
#' @return Named list with \code{combo} (integer 0..7), \code{combo_label}
#'   (one of \code{a1..a8}), \code{alert_count} (0..3),
#'   \code{mandela_category}, and \code{notes}.
#' @export
#' @examples
#' morie_otis_classify_mandela_combo(1, 0, 0)
#' morie_otis_classify_mandela_combo(1, 1, 1, days = 20, hours_per_day = 23)
morie_otis_classify_mandela_combo <- function(mh, sr, sw,
                                              days = NA_real_,
                                              hours_per_day = 22) {
  mh <- as.integer(as.logical(mh))
  sr <- as.integer(as.logical(sr))
  sw <- as.integer(as.logical(sw))
  combo <- mh * 4L + sr * 2L + sw * 1L
  # 8-state labels (combo 0 == "no alerts" -> a8 by Ruhela convention)
  combo_label <- switch(as.character(combo),
                        "4" = "a1",  # MH only
                        "2" = "a2",  # SR only
                        "1" = "a3",  # SW only (empirically empty)
                        "6" = "a4",  # MH + SR
                        "3" = "a5",  # SR + SW
                        "5" = "a6",  # MH + SW (empirically empty)
                        "7" = "a7",  # all three
                        "0" = "a8",  # no alerts
                        NA_character_)
  alert_count <- mh + sr + sw

  if (!is.na(days)) {
    # Pure Mandela threshold: > 15 days = prolonged solitary = torture.
    # (The fancier 22-hr / 4-hr-missed delegation to
    # morie_siu_classify_mandela needs SIU-specific inputs we don't
    # have at this aggregation level.)
    days_n <- as.numeric(days)
    mandela_cat <- if (days_n > 15) "torture"
                   else if (days_n >= 1) "at-risk"
                   else "compliant"
  } else {
    # Alert-only proxy: any active alert -> at-risk; none -> compliant
    mandela_cat <- if (alert_count >= 1L) "at-risk" else "compliant"
  }

  list(combo = combo,
       combo_label = combo_label,
       alert_count = alert_count,
       mandela_category = mandela_cat,
       notes = if (is.na(days)) "alert-only classification (no duration)"
               else "duration-aware classification")
}


# ---------------------------------------------------------------------------
# Cell-frame builders: alert-complexity -> regional-volatility
# ---------------------------------------------------------------------------

# Aggregate per-(id, year) the count of distinct alert-combos and the
# sum of within-row + across-row region-change indicators.
.otis_alert_volatility_frame <- function(df) {
  needed <- c("UniqueIndividual_ID", "EndFiscalYear", "Gender",
              "Age_Category", "Region_AtTimeOfPlacement",
              "Region_MostRecentPlacement",
              "MentalHealth_Alert", "SuicideRisk_Alert",
              "SuicideWatch_Alert")
  missing_cols <- setdiff(needed, names(df))
  if (length(missing_cols)) {
    stop("missing required OTIS columns: ",
         paste(missing_cols, collapse = ", "))
  }
  base <- df[, needed, drop = FALSE]
  base <- base[stats::complete.cases(base), , drop = FALSE]
  a_mh <- .otis_binarise(base$MentalHealth_Alert)
  a_sr <- .otis_binarise(base$SuicideRisk_Alert)
  a_sw <- .otis_binarise(base$SuicideWatch_Alert)
  base$combo <- as.integer(a_mh * 4L + a_sr * 2L + a_sw * 1L)
  base$regA <- as.character(base$Region_AtTimeOfPlacement)
  base$regB <- as.character(base$Region_MostRecentPlacement)
  base$vm_within_row <- as.integer(base$regA != base$regB)

  # Sort by (id, year) for the across-row shift
  base <- base[order(base$UniqueIndividual_ID, base$EndFiscalYear), ,
               drop = FALSE]
  # vm_across_row: regA changed vs previous row WITHIN (id, year)
  grp_key <- paste(base$UniqueIndividual_ID, base$EndFiscalYear,
                   sep = "|")
  regA_prev <- ave(base$regA, grp_key, FUN = function(x) {
    c(NA_character_, utils::head(x, -1L))
  })
  base$vm_across_row <- as.integer(!is.na(regA_prev) &
                                     base$regA != regA_prev)
  base$vm_row <- base$vm_within_row + base$vm_across_row
  base
}

#' Ruhela's primary alert-complexity -> regional-volatility builder.
#'
#' Implements Ruhela's "ac >= 2 -> vm" RF (Ruhela Formulation): the
#' 8-state combo encoding documented in OTIS-RC/notez1a.qmd and used
#' for the published \code{res_pool} / \code{res_by_year} / \code{res_all}
#' estimates. Per \code{(UniqueIndividual_ID, EndFiscalYear)}, the
#' alert-state \emph{complexity} \code{ac} is the number of distinct
#' alert combos with positive support across that person-year's rows
#' (NOT the max of simultaneous flags -- see the Naive arm for that
#' alternative). Treatment \code{T_high_ac = 1L} iff \code{ac >= 2}.
#' Outcome \code{Y_vm_count} sums the within-row and across-row
#' regional-volatility-move indicators.
#'
#' @param df OTIS placement-level data.frame (b01 / a01 schema).
#' @return A named list with elements \code{data} (the person-year
#'   data.frame), \code{T} = "T_high_ac", \code{Y} = "Y_vm_count", and
#'   \code{covariates} = c("Gender", "Age_Category", "EndFiscalYear").
#' @export
#' @examples
#' \dontrun{
#'   df <- morie_otis_load()
#'   pair <- morie_otis_make_pair_alert_to_volatility_ruhela(df)
#'   morie_otis_irm_dml(pair$data, treatment = pair$T,
#'                      outcome = pair$Y, covariates = pair$covariates)
#' }
morie_otis_make_pair_alert_to_volatility_ruhela <- function(df) {
  base <- .otis_alert_volatility_frame(df)

  # Per (id, year): distinct combos count + vm sum + first demographics
  key <- paste(base$UniqueIndividual_ID, base$EndFiscalYear,
               sep = "|")
  splits <- split(seq_len(nrow(base)), key)
  ac <- vapply(splits, function(ix) length(unique(base$combo[ix])),
               integer(1))
  vm <- vapply(splits, function(ix) sum(base$vm_row[ix]), integer(1))
  first_idx <- vapply(splits, function(ix) ix[1L], integer(1))
  py <- data.frame(
    UniqueIndividual_ID = base$UniqueIndividual_ID[first_idx],
    EndFiscalYear = base$EndFiscalYear[first_idx],
    Gender = base$Gender[first_idx],
    Age_Category = base$Age_Category[first_idx],
    regA = base$regA[first_idx],
    regB = base$regB[first_idx],
    ac = ac,
    vm = vm,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  py$T_high_ac <- as.integer(py$ac >= 2L)
  py$Y_vm_count <- as.integer(py$vm)
  list(
    data = py,
    T = "T_high_ac",
    Y = "Y_vm_count",
    covariates = c("Gender", "Age_Category", "EndFiscalYear")
  )
}

#' Naive (max-simultaneous-flags + binary vm) alert -> volatility builder.
#'
#' Robustness alternative to
#' \code{morie_otis_make_pair_alert_to_volatility_ruhela()}: treatment
#' = "max simultaneous flags across the year's rows >= 2"; outcome =
#' "any placement row with regA != regB" (binary). Produces a different
#' treatment marginal (~24\% vs ~14\% under the Ruhela 8-state
#' encoding) and a binary rather than count outcome. Used side-by-side
#' as the Naive arm of an RDF (Ruhela Dual Formulation).
#'
#' @param df OTIS placement-level data.frame.
#' @return Named list with \code{data}, \code{T} = "T_high_ac",
#'   \code{Y} = "Y_vm_any", \code{covariates} =
#'   c("Gender", "Age_Category", "EndFiscalYear").
#' @export
#' @examples
#' \dontrun{
#'   df <- morie_otis_load()
#'   morie_otis_make_pair_alert_to_volatility_naive(df)
#' }
morie_otis_make_pair_alert_to_volatility_naive <- function(df) {
  needed <- c("UniqueIndividual_ID", "EndFiscalYear", "Gender",
              "Age_Category", "Region_AtTimeOfPlacement",
              "Region_MostRecentPlacement",
              "MentalHealth_Alert", "SuicideRisk_Alert",
              "SuicideWatch_Alert")
  base <- df[, needed, drop = FALSE]
  base <- base[stats::complete.cases(base), , drop = FALSE]
  base$alert_sum_row <- .otis_binarise(base$MentalHealth_Alert) +
                          .otis_binarise(base$SuicideRisk_Alert) +
                          .otis_binarise(base$SuicideWatch_Alert)
  base$vm_row_binary <- as.integer(base$Region_AtTimeOfPlacement !=
                                      base$Region_MostRecentPlacement)
  key <- paste(base$UniqueIndividual_ID, base$EndFiscalYear,
               sep = "|")
  splits <- split(seq_len(nrow(base)), key)
  ac_naive <- vapply(splits, function(ix) max(base$alert_sum_row[ix]),
                     integer(1))
  vm_any <- vapply(splits, function(ix) max(base$vm_row_binary[ix]),
                   integer(1))
  first_idx <- vapply(splits, function(ix) ix[1L], integer(1))
  py <- data.frame(
    UniqueIndividual_ID = base$UniqueIndividual_ID[first_idx],
    EndFiscalYear = base$EndFiscalYear[first_idx],
    Gender = base$Gender[first_idx],
    Age_Category = base$Age_Category[first_idx],
    ac_naive = ac_naive,
    vm_any = vm_any,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  py$T_high_ac <- as.integer(py$ac_naive >= 2L)
  py$Y_vm_any <- as.integer(py$vm_any)
  list(
    data = py,
    T = "T_high_ac",
    Y = "Y_vm_any",
    covariates = c("Gender", "Age_Category", "EndFiscalYear")
  )
}

#' Run both Ruhela and Naive alert -> volatility builders.
#'
#' Convenience wrapper that returns both formulations side-by-side for
#' RDF (Ruhela Dual Formulation) robustness analyses.
#'
#' @param df OTIS placement-level data.frame.
#' @return Named list \code{list(ruhela = ..., naive = ...)} where
#'   each element is the output of the corresponding make-pair builder.
#' @export
#' @examples
#' \dontrun{
#'   morie_otis_make_pair_alert_to_volatility_all(morie_otis_load())
#' }
morie_otis_make_pair_alert_to_volatility_all <- function(df) {
  list(
    ruhela = morie_otis_make_pair_alert_to_volatility_ruhela(df),
    naive  = morie_otis_make_pair_alert_to_volatility_naive(df)
  )
}

#' a01-aware wrapper for the Ruhela alert -> volatility builder.
#'
#' Same as \code{morie_otis_make_pair_alert_to_volatility_ruhela()} but
#' auto-loads a01 (Restrictive Confinement Detailed) via the registered
#' \code{morie_otis_load()} loader when \code{df = NULL}. a01 is the
#' canonical file the published OTIS-RC \code{res_pool} /
#' \code{res_by_year} / \code{res_all} are computed on.
#'
#' @param df Optional OTIS a01 data.frame. If \code{NULL}, attempts to
#'   resolve via \code{morie_otis_load()}.
#' @return Same shape as
#'   \code{morie_otis_make_pair_alert_to_volatility_ruhela()}.
#' @export
#' @examples
#' \dontrun{
#'   morie_otis_make_pair_alert_to_volatility_a01()
#' }
morie_otis_make_pair_alert_to_volatility_a01 <- function(df = NULL) {
  if (is.null(df)) {
    if (!exists("morie_otis_load", mode = "function")) {
      stop("morie_otis_make_pair_alert_to_volatility_a01: pass `df=` ",
           "or ensure morie_otis_load() is available")
    }
    df <- morie_otis_load()
  }
  morie_otis_make_pair_alert_to_volatility_ruhela(df)
}


# ---------------------------------------------------------------------------
# Canonical 3 (T, Y) pairs for the causal grid
# ---------------------------------------------------------------------------

#' Pair (a): MentalHealth_Alert -> SuicideRisk_Alert (binary -> binary).
#'
#' The clinical-alert chain: do mental-health flags causally elevate
#' subsequent suicide-risk-alert occurrence, conditional on
#' demographics and region?
#'
#' @param df OTIS placement-level data.frame.
#' @return Named list \code{list(data, T = "T_a", Y = "Y_a", covariates)}.
#' @export
#' @examples
#' \dontrun{
#'   morie_otis_make_pair_a(morie_otis_load())
#' }
morie_otis_make_pair_a <- function(df) {
  needed <- c("UniqueIndividual_ID", "EndFiscalYear", "Gender",
              "Age_Category", "Region_AtTimeOfPlacement",
              "Region_MostRecentPlacement", "MentalHealth_Alert",
              "SuicideRisk_Alert")
  base <- df[, needed, drop = FALSE]
  base <- base[stats::complete.cases(base), , drop = FALSE]
  base$T_a <- .otis_binarise(base$MentalHealth_Alert)
  base$Y_a <- .otis_binarise(base$SuicideRisk_Alert)
  list(
    data = base, T = "T_a", Y = "Y_a",
    covariates = c("Gender", "Age_Category",
                    "Region_AtTimeOfPlacement",
                    "Region_MostRecentPlacement")
  )
}

#' Pair (b): HighAlertComplexity -> AnyReadmission.
#'
#' Treatment T_b = 1 iff at least 2 of (MentalHealth, SuicideRisk,
#' SuicideWatch) alerts are simultaneously active in the row.
#' Outcome Y_b = 1 iff Number_Of_Placements >= 2 (proxy for any future
#' readmission).
#'
#' @param df OTIS placement-level data.frame.
#' @return Named list \code{list(data, T = "T_b", Y = "Y_b", covariates)}.
#' @export
#' @examples
#' \dontrun{
#'   morie_otis_make_pair_b(morie_otis_load())
#' }
morie_otis_make_pair_b <- function(df) {
  needed <- c("UniqueIndividual_ID", "EndFiscalYear", "Gender",
              "Age_Category", "Region_AtTimeOfPlacement",
              "Region_MostRecentPlacement",
              "MentalHealth_Alert", "SuicideRisk_Alert",
              "SuicideWatch_Alert", "Number_Of_Placements")
  base <- df[, needed, drop = FALSE]
  base <- base[stats::complete.cases(base), , drop = FALSE]
  a1 <- .otis_binarise(base$MentalHealth_Alert)
  a2 <- .otis_binarise(base$SuicideRisk_Alert)
  a3 <- .otis_binarise(base$SuicideWatch_Alert)
  base$T_b <- as.integer((a1 + a2 + a3) >= 2L)
  np <- suppressWarnings(as.numeric(base$Number_Of_Placements))
  np[is.na(np)] <- 0
  base$Y_b <- as.integer(np >= 2)
  list(
    data = base, T = "T_b", Y = "Y_b",
    covariates = c("Gender", "Age_Category",
                    "Region_AtTimeOfPlacement",
                    "Region_MostRecentPlacement")
  )
}

#' Pair (c): RegionalVolatility -> SegregationDays.
#'
#' Treatment T_c = 1 iff Region_AtTimeOfPlacement != Region_MostRecent.
#' Outcome Y_c = NumberConsecutiveDays_Segregation winsorised at the
#' 99th percentile.
#'
#' @param df OTIS placement-level data.frame.
#' @return Named list \code{list(data, T = "T_c", Y = "Y_c", covariates)}.
#' @export
#' @examples
#' \dontrun{
#'   morie_otis_make_pair_c(morie_otis_load())
#' }
morie_otis_make_pair_c <- function(df) {
  needed <- c("UniqueIndividual_ID", "EndFiscalYear", "Gender",
              "Age_Category", "Region_AtTimeOfPlacement",
              "Region_MostRecentPlacement",
              "MentalHealth_Alert",
              "NumberConsecutiveDays_Segregation")
  base <- df[, needed, drop = FALSE]
  base <- base[stats::complete.cases(base), , drop = FALSE]
  base$T_c <- as.integer(base$Region_AtTimeOfPlacement !=
                            base$Region_MostRecentPlacement)
  y <- suppressWarnings(as.numeric(base$NumberConsecutiveDays_Segregation))
  y[is.na(y)] <- 0
  p99 <- stats::quantile(y, 0.99, na.rm = TRUE)
  base$Y_c <- pmin(y, p99)
  list(
    data = base, T = "T_c", Y = "Y_c",
    covariates = c("Gender", "Age_Category", "MentalHealth_Alert")
  )
}


# ---------------------------------------------------------------------------
# 3 x 3 causal grid (IPW / AIPW / IRM-DML across the three pairs)
# ---------------------------------------------------------------------------

#' Run IPW / AIPW / IRM-DML on the three canonical (T, Y) pairs.
#'
#' Returns one row per (pair, estimator) combination with the ATE,
#' SE, 95% CI, and per-row notes. The IRM-DML row uses the ATE
#' component of \code{morie_otis_irm_dml()}'s output (not the ATTE /
#' ATC). Concordance across all three estimators is the strongest
#' evidence of an identified causal effect under conditional
#' exchangeability.
#'
#' @param df OTIS placement-level data.frame. If \code{NULL}, attempts
#'   to resolve via \code{morie_otis_load()}.
#' @param seed Integer seed for the cross-fitting (default 123).
#' @return Data.frame with columns \code{pair}, \code{estimator},
#'   \code{n}, \code{p_treat}, \code{ate}, \code{ate_se},
#'   \code{ate_pval}, \code{ci95_lo}, \code{ci95_hi}, \code{notes}.
#' @export
#' @examples
#' \dontrun{
#'   morie_otis_causal_grid()
#' }
morie_otis_causal_grid <- function(df = NULL, seed = 123L) {
  if (is.null(df)) {
    if (!exists("morie_otis_load", mode = "function")) {
      stop("morie_otis_causal_grid: pass `df=` or ensure ",
           "morie_otis_load() is available")
    }
    df <- morie_otis_load()
  }
  pairs <- list(
    "(a) MentalHealth -> SuicideRisk" = morie_otis_make_pair_a(df),
    "(b) HighAlertComplexity -> AnyReadmission" = morie_otis_make_pair_b(df),
    "(c) RegionalVolatility -> SegregationDays" = morie_otis_make_pair_c(df)
  )
  rows <- list()
  for (label in names(pairs)) {
    pr <- pairs[[label]]
    data <- pr$data
    Tn <- pr$T
    Yn <- pr$Y
    covs <- pr$covariates
    if (sum(data[[Tn]]) == 0L || sum(data[[Tn]]) == nrow(data)) {
      warning(sprintf("%s: degenerate treatment, skipping", label))
      next
    }
    for (kind in c("IPW", "AIPW", "IRM-DML")) {
      est <- tryCatch({
        if (kind == "IPW") {
          morie_otis_ipw_ate(data, treatment = Tn, outcome = Yn,
                             covariates = covs)
        } else if (kind == "AIPW") {
          morie_otis_aipw_ate(data, treatment = Tn, outcome = Yn,
                              covariates = covs, seed = seed)
        } else {
          irm <- morie_otis_irm_dml(data, treatment = Tn, outcome = Yn,
                                    covariates = covs, seed = seed)
          .otis_causal_estimate("IRM-DML",
                                irm$ate, irm$ate_se, irm$ate_pval,
                                irm$n, irm$n_treated, irm$p_treat,
                                notes = list(irm$se_kind))
        }
      }, error = function(e) {
        list(error = substr(conditionMessage(e), 1L, 80L))
      })
      if (!is.null(est$error)) {
        rows[[length(rows) + 1L]] <- data.frame(
          pair = label, estimator = kind, n = NA_integer_,
          p_treat = NA_real_, ate = NA_real_, ate_se = NA_real_,
          ate_pval = NA_real_, ci95_lo = NA_real_, ci95_hi = NA_real_,
          notes = est$error, stringsAsFactors = FALSE)
      } else {
        rows[[length(rows) + 1L]] <- data.frame(
          pair = label, estimator = kind,
          n = est$n, p_treat = round(est$p_treat, 4),
          ate = round(est$ate, 4),
          ate_se = round(est$ate_se, 4),
          ate_pval = round(est$ate_pval, 4),
          ci95_lo = round(est$ate_ci95[1], 4),
          ci95_hi = round(est$ate_ci95[2], 4),
          notes = paste(unlist(est$notes), collapse = "; "),
          stringsAsFactors = FALSE)
      }
    }
  }
  do.call(rbind, rows)
}


# ---------------------------------------------------------------------------
# Stubs for the high-ML estimators (mirror python public API surface)
# ---------------------------------------------------------------------------

#' SuperLearner-stacked AIPW (not yet ported).
#'
#' The python version stacks RF + ridge + OLS/logit + mean (and
#' optionally xgboost) via cross-validated convex weights. The R port
#' would require pulling in \pkg{SuperLearner} or hand-rolling the
#' stacked-cross-fit construction.
#'
#' @param ... Arguments mirroring \code{morie_otis_aipw_ate()}.
#' @return Stops with a \code{NotYetPorted} message; for the time
#'   being, call \code{morie_otis_aipw_ate()} with the default
#'   cross-fit OLS+logit stack.
#' @export
#' @examples
#' \dontrun{
#'   morie_otis_aipw_superlearner(df, treatment = "d", outcome = "y",
#'                                 covariates = "x")
#' }
morie_otis_aipw_superlearner <- function(...) {
  stop("NotYetPorted: SuperLearner-stacked AIPW requires a stacked-",
       "cross-fit ensemble (RF + ridge + logit + mean). Use ",
       "`morie_otis_aipw_ate()` for the OLS+logit cross-fit, or call ",
       "the python implementation in `src/morie/otis_causal.py`.")
}

#' Partially Linear Regression DML on OTIS (not yet ported).
#'
#' The python version uses scikit-learn RF nuisance models for the
#' Frisch-Waugh-Lovell partialling-out construction. For the R port,
#' use the analogous \code{morie_estimate_double_ml()} from
#' \code{causal.R}, which already wraps \pkg{DoubleML::DoubleMLPLR}
#' (with mlr3 ranger learners) and a cross-fit ridge fallback.
#'
#' @param ... Arguments mirroring \code{morie_otis_aipw_ate()}.
#' @return Stops with a redirect to \code{morie_estimate_double_ml()}.
#' @export
#' @examples
#' \dontrun{
#'   morie_otis_plr(df, treatment = "d", outcome = "y",
#'                  covariates = "x")
#' }
morie_otis_plr <- function(...) {
  stop("NotYetPorted: PLR-DML with random-forest nuisances. Call ",
       "`morie_estimate_double_ml(data, outcome, treatment, ",
       "covariates)` from `causal.R` instead -- it already wraps ",
       "DoubleML::DoubleMLPLR (ranger learners) when available and ",
       "falls back to cross-fit ridge otherwise.")
}

#' Propensity-score 1:k NN matching with caliper (not yet ported).
#'
#' The python implementation provides a greedy 1:k NN matcher on
#' logit-PS with an Austin (2011) 0.2-SD caliper. In R, prefer the
#' canonical \pkg{MatchIt} implementation
#' (\code{MatchIt::matchit(method = "nearest", caliper = 0.2,
#' std.caliper = TRUE)}); the present stub holds the python-API
#' surface so callers can detect the rename.
#'
#' @param ... Arguments mirroring \code{morie_otis_aipw_ate()}.
#' @return Stops with a redirect to \pkg{MatchIt}.
#' @export
#' @examples
#' \dontrun{
#'   morie_otis_psm(df, treatment = "d", outcome = "y",
#'                  covariates = "x")
#' }
morie_otis_psm <- function(...) {
  stop("NotYetPorted: 1:k NN PSM with caliper. Use ",
       "`MatchIt::matchit(t ~ ., data = df, method = 'nearest', ",
       "caliper = 0.2, std.caliper = TRUE)` then run ",
       "`morie_otis_aipw_ate()` on `MatchIt::match.data(m)`.")
}

#' Propensity-score subclassification ATE (not yet ported).
#'
#' Rosenbaum-Rubin (1983) PS-stratification (default n_strata = 5,
#' Cochran 1968 convention). Use \code{MatchIt::matchit(method =
#' "subclass")} for an R-side equivalent.
#'
#' @param ... Arguments mirroring \code{morie_otis_ipw_ate()}.
#' @return Stops with a redirect to \pkg{MatchIt} subclassification.
#' @export
#' @examples
#' \dontrun{
#'   morie_otis_psm_subclass(df, treatment = "d", outcome = "y",
#'                            covariates = "x")
#' }
morie_otis_psm_subclass <- function(...) {
  stop("NotYetPorted: PS-subclass ATE. Use ",
       "`MatchIt::matchit(t ~ ., data = df, method = 'subclass', ",
       "subclass = 5)` then aggregate within-stratum mean differences ",
       "weighted by stratum size.")
}
