#' Causal effect estimators: IPW regression, g-computation, 2SLS, PLIV, sequential matching, SNFTM
#'
#' R arm of \code{morie.fn.ate}, \code{g_comp}, \code{late}, \code{pliv},
#' \code{prsmtd} and \code{snmcox}.
#'
#' \code{Ate}: weighted least squares of the outcome on \eqn{(1, T)}; the
#' treatment coefficient is the Hajek IPW contrast, with the HC3 sandwich of
#' the weighted fit as its standard error.
#'
#' \code{GComp}: g-computation. An outcome model \eqn{E[Y | T, X]} (least
#' squares, or unpenalised logistic regression by Newton's method) is fitted
#' and the ATE is the mean of \eqn{\hat Y(1) - \hat Y(0)}; the standard error is
#' the standard deviation of 500 bootstrap replicates (replicate \eqn{b}
#' resamples rows from Philox stream \eqn{b} of seed 42; single-arm resamples
#' are skipped), the interval their type-7 2.5\% and 97.5\% quantiles.
#'
#' \code{Late}: two-stage least squares of \eqn{Y} on \eqn{(1, W, T)} with
#' instruments \eqn{(1, W, Z)}; homoskedastic or HC0 standard error and the
#' first-stage F statistic of the excluded instrument.
#'
#' \code{Pliv}: partially linear IV by double machine learning. Ridge
#' nuisances (penalty chosen from 0.1, 1, 10 by leave-one-out PRESS) are
#' cross-fitted over folds dealt from the Philox order of seed
#' \code{random_state}; \eqn{\theta = \sum w u / \sum w v} with the
#' partialling-out score.
#'
#' \code{Prsmtd}: sequential risk-set propensity matching. In each period a
#' logistic propensity of initiation on the current covariate is fitted in the
#' risk set, and initiators (in decreasing propensity) take the nearest unused
#' not-yet-treated control.
#'
#' @param data A data frame (or list of columns).
#' @param outcome,treatment,instrument Column names.
#' @param weights_col Column of analytic weights.
#' @param covariates Covariate column names.
#' @param outcome_model \code{"linear"} or \code{"logistic"}.
#' @param se_type \code{"homoskedastic"} or \code{"robust"} (HC0).
#' @param n_folds Cross-fitting folds.
#' @param random_state Philox seed of the fold assignment.
#' @param A,H Treatment-initiation indicators and covariate, n by T matrices (or vectors).
#' @return A named list (the Python result's fields). \code{Prsmtd} reports 1-based periods and rows.
#' @references Hernan, M. A. and Robins, J. M. (2020). Causal Inference: What If. Chapman & Hall/CRC.
#'
#'   MacKinnon, J. G. and White, H. (1985). Some heteroskedasticity-consistent covariance matrix
#'   estimators with improved finite sample properties. Journal of Econometrics 29(3), 305-325.
#'
#'   Robins, J. M. (1986). A new approach to causal inference in mortality studies with a sustained
#'   exposure period. Mathematical Modelling 7, 1393-1512.
#'
#'   Imbens, G. W. and Angrist, J. D. (1994). Identification and estimation of local average
#'   treatment effects. Econometrica 62(2), 467-475.
#'
#'   Chernozhukov, V. et al. (2018). Double/debiased machine learning for treatment and structural
#'   parameters. Econometrics Journal 21(1), C1-C68.
#'
#'   Lu, B. (2005). Propensity score matching with time-dependent covariates. Biometrics 61(3), 721-728.
#'
#'   Robins, J. M. (1992). Estimation of the time-dependent accelerated failure time model in the
#'   presence of confounding factors. Biometrika 79(2), 321-334.
#' @examples
#' d <- data.frame(
#'   y = c(1, 2.2, 1.7, 3.1, 2.8, 3.9),
#'   t = c(0, 0, 0, 1, 1, 1),
#'   w = c(1.2, 2, 1.5, 1.1, 3, 1.4)
#' )
#' Ate(d, "y", "t", "w")
#' iv <- list(z = c(0, 0, 0, 0, 1, 1, 1, 1), t = c(0, 0, 1, 0, 1, 1, 0, 1),
#'            y = c(1, 1.4, 3.1, 0.8, 3.3, 2.9, 1.2, 3.6))
#' Late(iv, "t", "y", "z")$late
#' @export
Ate <- function(data, outcome, treatment, weights_col) {
  y <- as.numeric(data[[outcome]])
  t <- as.numeric(data[[treatment]])
  w <- as.numeric(data[[weights_col]])
  ok <- !(is.na(y) | is.na(t) | is.na(w))
  sw <- sqrt(w[ok])
  Xw <- cbind(1, t[ok]) * sw
  yw <- y[ok] * sw
  B <- solve(crossprod(Xw))
  beta <- drop(B %*% crossprod(Xw, yw))
  ew <- yw - drop(Xw %*% beta)
  h <- rowSums((Xw %*% B) * Xw)
  V <- B %*% crossprod(Xw * (ew / (1 - h))) %*% B
  list(ate = beta[2], se = sqrt(V[2, 2]))
}

.cst_logit_newton <- function(X, y, max_iter = 500) {
  b <- rep(0, ncol(X))
  for (it in seq_len(max_iter)) {
    p <- 1 / (1 + exp(-drop(X %*% b)))
    g <- drop(crossprod(X, p - y))
    H <- crossprod(X * (p * (1 - p)), X) + diag(1e-10, ncol(X))
    step <- solve(H, g)
    b <- b - step
    if (max(abs(step)) < 1e-10) break
  }
  b
}

.cst_gcomp_ate <- function(t, X, y, outcome_model) {
  D <- cbind(1, t, X)
  D1 <- cbind(1, 1, X)
  D0 <- cbind(1, 0, X)
  if (outcome_model == "linear") {
    b <- solve(crossprod(D), crossprod(D, y))
    return(mean(drop(D1 %*% b) - drop(D0 %*% b)))
  }
  if (length(unique(y)) < 2) stop("needs samples of at least 2 classes")
  b <- .cst_logit_newton(D, y)
  mean(1 / (1 + exp(-drop(D1 %*% b))) - 1 / (1 + exp(-drop(D0 %*% b))))
}

#' @rdname Ate
#' @export
GComp <- function(data, treatment, outcome, covariates, outcome_model = "linear") {
  if (!outcome_model %in% c("linear", "logistic")) stop("outcome_model must be 'linear' or 'logistic'")
  cols <- c(treatment, outcome, covariates)
  M <- sapply(cols, function(cn) as.numeric(data[[cn]]))
  M <- M[stats::complete.cases(M), , drop = FALSE]
  n <- nrow(M)
  if (n < 10) stop("G-computation requires at least 10 complete observations.")
  t <- M[, 1]
  y <- M[, 2]
  X <- M[, -(1:2), drop = FALSE]
  ate <- .cst_gcomp_ate(t, X, y, outcome_model)
  boot <- numeric(0)
  for (b in 0:499) {
    idx <- pmin(floor(.morie_random_uniform(n, 42, b) * n), n - 1) + 1
    if (length(unique(t[idx])) < 2) next
    v <- tryCatch(.cst_gcomp_ate(t[idx], X[idx, , drop = FALSE], y[idx], outcome_model), error = function(e) NULL)
    if (!is.null(v)) boot <- c(boot, v)
  }
  q <- stats::quantile(boot, c(0.025, 0.975), names = FALSE)
  list(ate = ate, se = stats::sd(boot), ci_lower = q[1], ci_upper = q[2], n_obs = n, outcome_model = outcome_model)
}

.cst_rss <- function(Z, t) {
  g <- solve(crossprod(Z), crossprod(Z, t))
  sum((t - drop(Z %*% g))^2)
}

#' @rdname Ate
#' @export
Late <- function(data, treatment, outcome, instrument, covariates = NULL, se_type = "homoskedastic") {
  if (!se_type %in% c("homoskedastic", "robust")) stop("se_type must be 'homoskedastic' or 'robust'")
  cols <- c(treatment, outcome, instrument, covariates)
  M <- sapply(cols, function(cn) as.numeric(data[[cn]]))
  M <- M[stats::complete.cases(M), , drop = FALSE]
  n <- nrow(M)
  W <- M[, -(1:3), drop = FALSE]
  X <- cbind(1, W, M[, 1])
  Z <- cbind(1, W, M[, 3])
  k <- ncol(X)
  if (n <= k) stop(n, " complete rows cannot support ", k, " parameters")
  rss_u <- .cst_rss(Z, M[, 1])
  rss_r <- .cst_rss(Z[, -k, drop = FALSE], M[, 1])
  if (rss_r - rss_u <= 1e-12 * rss_r) {
    stop("Instrument has no partial correlation with treatment; LATE is not identified (weak instrument).")
  }
  Xh <- Z %*% solve(crossprod(Z), crossprod(Z, X))
  B <- solve(crossprod(Xh))
  beta <- drop(B %*% crossprod(Xh, M[, 2]))
  e <- M[, 2] - drop(X %*% beta)
  v <- if (se_type == "homoskedastic") sum(e^2) / (n - k) * B[k, k] else sum((drop(Xh %*% B[, k]) * e)^2)
  se <- sqrt(v)
  zc <- stats::qnorm(0.975)
  list(late = unname(beta[k]), se = se, ci = unname(c(beta[k] - zc * se, beta[k] + zc * se)),
       f_stat = (rss_r - rss_u) / (rss_u / (n - k)), n = n, se_type = se_type,
       method = if (length(covariates)) "2SLS with covariates" else "2SLS (Wald ratio)")
}

.cst_ridge <- function(X, y, alpha) {
  xm <- colMeans(X)
  Xc <- sweep(X, 2, xm)
  coef <- solve(crossprod(Xc) + diag(alpha, ncol(X)), crossprod(Xc, y - mean(y)))
  list(coef = drop(coef), intercept = mean(y) - sum(coef * xm))
}

.cst_ridgecv <- function(X, y, alphas = c(0.1, 1, 10)) {
  n <- nrow(X)
  Xc <- sweep(X, 2, colMeans(X))
  best <- Inf
  best_alpha <- alphas[1]
  for (a in alphas) {
    f <- .cst_ridge(X, y, a)
    pred <- f$intercept + drop(X %*% f$coef)
    h <- 1 / n + rowSums((Xc %*% solve(crossprod(Xc) + diag(a, ncol(X)))) * Xc)
    press <- sum(((y - pred) / pmax(1 - h, 1e-10))^2)
    if (press < best) {
      best <- press
      best_alpha <- a
    }
  }
  .cst_ridge(X, y, best_alpha)
}

#' @rdname Ate
#' @export
Pliv <- function(data, treatment, outcome, instrument, covariates, n_folds = 5, random_state = 42) {
  cols <- c(treatment, outcome, instrument, covariates)
  M <- sapply(cols, function(cn) as.numeric(data[[cn]]))
  M <- M[stats::complete.cases(M), , drop = FALSE]
  n <- nrow(M)
  d <- M[, 1]
  y <- M[, 2]
  z <- M[, 3]
  X <- M[, -(1:3), drop = FALSE]
  ord <- order(.morie_random_uniform(n, random_state, 0))
  lhat <- mhat <- rhat <- numeric(n)
  for (f in seq_len(n_folds)) {
    fold <- ord[seq(f, n, by = n_folds)]
    train <- setdiff(seq_len(n), fold)
    for (s in list(list("l", y), list("m", z), list("r", d))) {
      v <- s[[2]]
      p <- if (ncol(X)) {
        fit <- .cst_ridgecv(X[train, , drop = FALSE], v[train])
        fit$intercept + drop(X[fold, , drop = FALSE] %*% fit$coef)
      } else {
        rep(mean(v[train]), length(fold))
      }
      if (s[[1]] == "l") lhat[fold] <- p else if (s[[1]] == "m") mhat[fold] <- p else rhat[fold] <- p
    }
  }
  u <- y - lhat
  w <- z - mhat
  v <- d - rhat
  wv <- sum(w * v)
  if (wv == 0) stop("instrument residual is orthogonal to the treatment residual")
  late <- sum(w * u) / wv
  psi <- (u - late * v) * w
  se <- sqrt(sum(psi^2) / n / (wv / n)^2 / n)
  zc <- 1.959963984540054
  list(late = late, se = se, ci_lower = late - zc * se, ci_upper = late + zc * se,
       pval = 2 * stats::pnorm(-abs(late / se)), n_obs = n,
       method = "PLIV (native DML, cross-fitted ridge nuisances)")
}

.cst_logit_fit <- function(x, y) {
  D <- cbind(1, x)
  beta <- rep(0, ncol(D))
  for (it in 1:100) {
    p <- 1 / (1 + exp(-pmin(pmax(drop(D %*% beta), -35), 35)))
    W <- pmax(p * (1 - p), 1e-10)
    step <- solve(crossprod(D * W, D), crossprod(D, y - p))
    beta <- beta + drop(step)
    if (max(abs(step)) < 1e-9) break
  }
  1 / (1 + exp(-pmin(pmax(drop(D %*% beta), -35), 35)))
}

#' @rdname Ate
#' @export
Prsmtd <- function(A, H) {
  A <- as.matrix(A)
  H <- as.matrix(H)
  if (!identical(dim(A), dim(H))) stop("A and H must share shape")
  if (!all(A %in% c(0, 1))) stop("A must be binary 0/1.")
  n <- nrow(A)
  ever <- used <- rep(FALSE, n)
  ps <- matrix(NA_real_, n, ncol(A))
  pairs <- matrix(integer(0), 0, 3)
  n_init <- 0
  for (tt in seq_len(ncol(A))) {
    risk <- !ever
    a_t <- A[, tt] == 1 & risk
    n_init <- n_init + sum(a_t)
    if (sum(risk) >= 4 && sum(a_t) > 0 && sum(a_t) < sum(risk)) {
      idx <- which(risk)
      ps[idx, tt] <- pmin(pmax(.cst_logit_fit(H[idx, tt], A[idx, tt]), 1e-6), 1 - 1e-6)
      init <- idx[A[idx, tt] == 1]
      controls <- idx[A[idx, tt] == 0 & !used[idx]]
      for (i in init[order(-ps[init, tt])]) {
        if (!length(controls)) break
        k <- which.min(abs(ps[controls, tt] - ps[i, tt]))
        j <- controls[k]
        controls <- controls[-k]
        used[j] <- TRUE
        pairs <- rbind(pairs, c(tt, i, j))
      }
    }
    ever <- ever | A[, tt] == 1
  }
  list(matched_idx = pairs, n_matched = nrow(pairs), n_initiators = n_init, propensity = ps, n = n,
       method = "Sequential propensity-score matching (period risk sets)")
}
