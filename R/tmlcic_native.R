# morie.fn -- function file (rootcoder007/morie)
# Adaptive pre-specification: cluster-randomized trial inference.
#
# In a cluster randomized trial the unit is the community, so there are
# often only a few dozen independent units and many baseline covariates
# that might be worth adjusting for. Adjusting well buys precision;
# adjusting badly overfits and inflates the type I error; choosing
# after seeing the results is a fishing expedition.
#
# Adaptive pre-specification: pre-specify a library of candidate working
# models and a rule for choosing among them. The rule follows empirical
# efficiency maximization -- the loss is the squared influence curve of
# the TMLE, so its risk is that estimator's asymptotic variance, and
# the selected candidate has the smallest cross-validated variance.
#
# The loss must match the design. The paired losses subtract the
# within-pair covariance the design already bought, so a covariate
# matched on perfectly earns no credit.
#
# Two targets: the population effect E[Y1-Y0] (influence curve D^P,
# eq. 13.3) and the sample effect (1/n) sum (Y1-Y0) (D^S, eq. 13.4,
# dropping the covariate-distribution term).
#
# Hierarchical data: tmle_hierarchical implements Balzer et al. (2019)'s
# two estimators (cluster-level TMLE I and individual-level TMLE II).
#
# References
# ----------
# Balzer, L. B., van der Laan, M. J. & Petersen, M. L. (2018)
# "Data-Adaptive Estimation in Cluster Randomized Trials", Ch. 13 in
# Targeted Learning in Data Science, Springer, pp. 195-215,
# doi:10.1007/978-3-319-65304-4_13.
#
# Balzer, L. B., van der Laan, M. J. & Petersen, M. L. (2016) "Adaptive
# pre-specification in randomized trials with and without
# pair-matching", Statistics in Medicine 35(25), 4528-4545,
# doi:10.1002/sim.7023.
#
# Balzer, L. B., Zheng, W., van der Laan, M. J. & Petersen, M. L.
# (2019) "A new approach to hierarchical data analysis", Statistical
# Methods in Medical Research 28(6), 1761-1780,
# doi:10.1177/0962280218774936.

.tmlcic_TARGETS <- c("SATE", "PATE")
.tmlcic_DESIGNS <- c("unmatched", "matched", "clustered")
.tmlcic_EPS <- 1e-9

#' .tmlcic_logit
#'
#' A step of the tmlcic_native implementation. Called by \code{.tmlcic_hier_cluster_arm},
#' \code{.tmlcic_hier_individual_arm}, \code{morie_tmlcic_candidate_tmle}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param p Coerced to numeric by the body, with \code{as.numeric}.
#' @return A numeric value.
#' @export
#' @examples
#' res <- .tmlcic_logit(p = 0.5)
#' res
.tmlcic_logit <- function(p) {
  q <- min(max(as.numeric(p), .tmlcic_EPS), 1.0 - .tmlcic_EPS)
  log(q / (1.0 - q))
}

#' Vectorised sigmoid (.s03sigmoid is scalar-only)
#'
#' A step of the tmlcic_native implementation. Called by \code{.tmlcic_fluct},
#' \code{.tmlcic_hier_cluster_arm}, \code{.tmlcic_hier_individual_arm} and 1 others in
#' the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param z Iterated over elementwise, with \code{vapply}.
#' @return A vector, from \code{vapply}.
#' @export
#' @examples
#' y <- c(2.9, 5.1, 6.8, 9.4, 11.2, 13.1, 15.0, 17.6)
#' res <- .tmlcic_sig(z = y)
#' res
.tmlcic_sig <- function(z) {
  # vectorised sigmoid (.s03sigmoid is scalar-only)
  vapply(z, .s03sigmoid, numeric(1))
}

#' Weighted logistic IRLS with a ridge penalty
#'
#' A step of the tmlcic_native implementation. Called by \code{.tmlcic_fit_g},
#' \code{.tmlcic_fit_working_model}, \code{.tmlcic_hier_cluster_arm} and 1 others in the
#' module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; passed to \code{nrow}.
#' @param y Numeric; combined arithmetically in the body.
#' @param ridge Numeric; passed to \code{max}. Defaults to \code{1e-10}.
#' @param obs_weights Optional; may be \code{NULL}. Coerced to numeric by the body, with
#' \code{as.numeric}.
#' @return The value of \code{beta}, as built in the body.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' y <- c(2.9, 5.1, 6.8, 9.4, 11.2, 13.1, 15.0, 17.6)
#' res <- .tmlcic_wlogit(X = x, y = y)
#' res
.tmlcic_wlogit <- function(X, y, ridge = 1e-10, obs_weights = NULL) {
  # Weighted logistic IRLS with a ridge penalty.
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  n <- nrow(X)
  p <- ncol(X)
  w <- if (is.null(obs_weights)) rep(1.0, n) else as.numeric(obs_weights)
  beta <- numeric(p)
  for (it in seq_len(60L)) {
    eta <- as.numeric(X %*% beta)
    mu <- .tmlcic_sig(eta)
    Wt <- w * mu * (1 - mu)
    XtWX <- crossprod(X, X * Wt) + diag(max(ridge, 1e-10), p)
    Xtr <- as.numeric(crossprod(X, w * (y - mu)))
    step <- tryCatch(solve(XtWX, Xtr), error = function(e) rep(0.0, p))
    beta <- beta + step
    if (max(abs(step)) < 1e-13) {
      break
    }
  }
  beta
}

#' One-parameter logistic fluctuation: solve
#'
#' sum w H (y - sigmoid(off + eps H)) = 0 for eps on the given rows.
#'
#' @param y A vector; its length is taken and its elements indexed.
#' @param off A vector; indexed elementwise.
#' @param H A vector; indexed elementwise.
#' @param rows Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @param obs_weights Optional; may be \code{NULL}. Coerced to numeric by the body, with
#' \code{as.numeric}.
#' @return The value of \code{eps}, as built in the body.
#' @export
.tmlcic_fluct <- function(y, off, H, rows = NULL, obs_weights = NULL) {
  # One-parameter logistic fluctuation: solve
  # sum w H (y - sigmoid(off + eps H)) = 0 for eps on the given rows.
  idx <- if (is.null(rows)) seq_along(y) else rows
  w <- if (is.null(obs_weights)) rep(1.0, length(y)) else as.numeric(obs_weights)
  eps <- 0.0
  for (it in seq_len(100L)) {
    p <- .tmlcic_sig(off[idx] + eps * H[idx])
    grad <- sum(w[idx] * H[idx] * (y[idx] - p))
    hess <- -sum(w[idx] * H[idx]^2 * p * (1 - p))
    if (abs(hess) < 1e-12) {
      break
    }
    delta <- grad / hess
    eps <- eps - delta
    if (abs(delta) < 1e-13) {
      break
    }
  }
  eps
}

#' The chapter\'s example library: the unadjusted model, one main term
#'
#' per covariate, and optionally one treatment interaction each. cols
#' are 0-based covariate indices.
#'
#' @param p A count; the body uses it as \code{seq_len(...)}.
#' @param interactions A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return The value of \code{lib}, as built in the body.
#' @export
morie_tmlcic_default_library <- function(p, interactions = TRUE) {
  # The chapter's example library: the unadjusted model, one main term
  # per covariate, and optionally one treatment interaction each. cols
  # are 0-based covariate indices.
  lib <- list(list(name = "unadjusted", cols = integer(0), interact = FALSE))
  for (j in seq_len(p)) {
    lib <- c(lib, list(list(
      name = sprintf("W%d", j), cols = (j - 1L),
      interact = FALSE
    )))
  }
  if (interactions) {
    for (j in seq_len(p)) {
      lib <- c(lib, list(list(
        name = sprintf("W%d x A", j), cols = (j - 1L),
        interact = TRUE
      )))
    }
  }
  lib
}

#' .tmlcic_row_fun
#'
#' A step of the tmlcic_native implementation. Called by \code{.tmlcic_fit_working_model}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param W A matrix; indexed by row and column.
#' @param cand A list; the body reads \code{$cols}, \code{$interact} from it.
#' @return The value of \code{function}.
#' @export
.tmlcic_row_fun <- function(W, cand) {
  cols1 <- cand$cols + 1L
  interact <- isTRUE(cand$interact)
  function(a, i) {
    base <- c(1.0, a, W[i, cols1])
    if (interact) {
      base <- c(base, a * W[i, cols1])
    }
    base
  }
}

#' Logit\[Qbar(A,W)\] on the candidate\'s terms, fitted on rows
#'
#' A step of the tmlcic_native implementation. Called by \code{morie_tmlcic_candidate_tmle}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y A vector; indexed elementwise.
#' @param A A vector; indexed elementwise.
#' @param W Passed to \code{.tmlcic_row_fun}.
#' @param cand Passed to \code{.tmlcic_row_fun}.
#' @param rows Iterated over elementwise, with \code{lapply}.
#' @param ridge Numeric; passed to \code{max}.
#' @return A list with \code{q}, \code{b}.
#' @export
.tmlcic_fit_working_model <- function(y, A, W, cand, rows, ridge) {
  # logit[Qbar(A,W)] on the candidate's terms, fitted on rows.
  rowf <- .tmlcic_row_fun(W, cand)
  X <- do.call(rbind, lapply(rows, function(i) rowf(A[i], i)))
  b <- .tmlcic_wlogit(X, y[rows], ridge = max(ridge, 1e-10))
  q <- function(a, i) .s03sigmoid(sum(b * rowf(a, i)))
  list(q = q, b = b)
}

#' A candidate for the exposure mechanism, P(A = 1 | W)
#'
#' A step of the tmlcic_native implementation. Called by
#' \code{morie_tmlcic_adaptive_prespecification}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A A vector; indexed elementwise.
#' @param W A matrix; indexed by row and column.
#' @param cand A list; the body reads \code{$cols} from it.
#' @param rows Iterated over elementwise, with \code{lapply}.
#' @param ridge Numeric; passed to \code{max}.
#' @return A list with \code{g1}, \code{b}.
#' @export
.tmlcic_fit_g <- function(A, W, cand, rows, ridge) {
  # A candidate for the exposure mechanism, P(A = 1 | W).
  cols1 <- cand$cols + 1L
  X <- do.call(rbind, lapply(rows, function(i) c(1.0, W[i, cols1])))
  b <- .tmlcic_wlogit(X, A[rows], ridge = max(ridge, 1e-10))
  g1 <- function(i) .s03sigmoid(sum(b * c(1.0, W[i, cols1])))
  list(g1 = g1, b = b)
}

#' morie_tmlcic_candidate_tmle
#'
#' A step of the tmlcic_native implementation. Called by
#' \code{morie_tmlcic_adaptive_prespecification}, \code{morie_tmlcic_tmle_cluster_ic}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y A vector; its length is taken.
#' @param A A vector; indexed elementwise.
#' @param W Passed to \code{.tmlcic_fit_working_model}.
#' @param cand Passed to \code{.tmlcic_fit_working_model}.
#' @param g1 Accepted by the signature and not used anywhere in the body.
#' @param rows Optional; may be \code{NULL}. Passed to \code{.tmlcic_fit_working_model}.
#' @param eval_rows Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @param ridge Passed to \code{.tmlcic_fit_working_model}. Defaults to \code{1e-08}.
#' @param target_step A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return A list with \code{q1}, \code{q0}, \code{qa}, \code{info}.
#' @export
morie_tmlcic_candidate_tmle <- function(y, A, W, cand, g1, rows = NULL,
                                        eval_rows = NULL, ridge = 1e-8,
                                        target_step = TRUE) {
  # One candidate TMLE: initial fit, targeting, predictions. Returns
  # list(q1, q0, qa, info) with the targeted predictions under
  # treatment and control and at the observed exposure.
  n <- length(y)
  rows <- if (is.null(rows)) seq_len(n) else rows
  eval_rows <- if (is.null(eval_rows)) seq_len(n) else eval_rows
  fm <- .tmlcic_fit_working_model(y, A, W, cand, rows, ridge)
  q <- fm$q
  gA <- numeric(n)
  H <- numeric(n)
  for (i in seq_len(n)) {
    p1 <- min(max(g1(i), .tmlcic_EPS), 1.0 - .tmlcic_EPS)
    gA[i] <- if (A[i] == 1.0) p1 else 1.0 - p1
    H[i] <- if (A[i] == 1.0) 1.0 / p1 else -1.0 / (1.0 - p1)
  }
  eps <- 0.0
  if (target_step) {
    off <- vapply(
      seq_len(n), function(i) .tmlcic_logit(q(A[i], i)),
      numeric(1)
    )
    eps <- .tmlcic_fluct(y, off, H, rows)
  }
  q1 <- rep(NA_real_, n)
  q0 <- rep(NA_real_, n)
  qa <- rep(NA_real_, n)
  for (i in eval_rows) {
    p1 <- min(max(g1(i), .tmlcic_EPS), 1.0 - .tmlcic_EPS)
    q1[i] <- .s03sigmoid(.tmlcic_logit(q(1.0, i)) + eps / p1)
    q0[i] <- .s03sigmoid(.tmlcic_logit(q(0.0, i)) - eps / (1.0 - p1))
    qa[i] <- if (A[i] == 1.0) q1[i] else q0[i]
  }
  list(q1 = q1, q0 = q0, qa = qa, info = list(eps = eps, gA = gA, H = H))
}

#' morie_tmlcic_influence_curve
#'
#' A step of the tmlcic_native implementation. Called by
#' \code{morie_tmlcic_adaptive_prespecification}, \code{morie_tmlcic_tmle_cluster_ic}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y A vector; its length is taken and its elements indexed.
#' @param A A vector; indexed elementwise.
#' @param q1 A vector; indexed elementwise.
#' @param q0 A vector; indexed elementwise.
#' @param qa A vector; indexed elementwise.
#' @param gA A vector; indexed elementwise.
#' @param rows See Usage.
#' @param psi Numeric; combined arithmetically in the body.
#' @param target Compared against \code{"SATE"}.
#' @return The value of \code{out}, as built in the body.
#' @export
morie_tmlcic_influence_curve <- function(y, A, q1, q0, qa, gA, rows, psi,
                                         target) {
  # Eq. (13.3) for the PATE and eq. (13.4) for the SATE.
  if (!(target %in% .tmlcic_TARGETS)) {
    stop(sprintf("tmlcic: target must be SATE or PATE, got %s", target))
  }
  out <- rep(NA_real_, length(y))
  for (i in rows) {
    sign_ <- if (A[i] == 1.0) 1.0 else -1.0
    resid <- sign_ * (y[i] - qa[i]) / gA[i]
    out[i] <- if (target == "SATE") {
      resid
    } else {
      resid + (q1[i] - q0[i]) - psi
    }
  }
  out
}

#' Group row indices by pair (or cluster) label, in first-seen order
#'
#' A step of the tmlcic_native implementation. Called by \code{morie_tmlcic_tmle_cluster_ic}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param cluster Optional; may be \code{NULL}. Coerced to character by the body, with
#' \code{as.character}.
#' @param n A count; the body uses it as \code{seq_len(...)}.
#' @return The value of \code{lapply}.
#' @export
.tmlcic_pairs_from <- function(cluster, n) {
  # Group row indices by pair (or cluster) label, in first-seen order.
  if (is.null(cluster)) {
    stop("tmlcic: a matched or clustered design needs the pair labels")
  }
  lab <- as.character(cluster)
  if (length(lab) != n) {
    stop(sprintf("tmlcic: %d pair labels for %d observations", length(lab), n))
  }
  order_ <- character(0)
  groups <- list()
  for (i in seq_len(n)) {
    key <- lab[i]
    if (is.null(groups[[key]])) {
      groups[[key]] <- integer(0)
      order_ <- c(order_, key)
    }
    groups[[key]] <- c(groups[[key]], i)
  }
  lapply(order_, function(c) groups[[c]])
}

#' morie_tmlcic_variance_estimate
#'
#' A step of the tmlcic_native implementation. Called by \code{morie_tmlcic_tmle_cluster_ic}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param D A vector; indexed elementwise.
#' @param y A vector; indexed elementwise.
#' @param qa A vector; indexed elementwise.
#' @param groups A vector; its length is taken.
#' @param n A count; the body uses it as \code{seq_len(...)}.
#' @param design Compared against \code{"unmatched"}.
#' @param target Compared against \code{"PATE"}.
#' @return A list with \code{var}, \code{info}.
#' @export
morie_tmlcic_variance_estimate <- function(D, y, qa, groups, n, design,
                                           target) {
  # The design's variance estimator. Returns list(var, info).
  if (design == "unmatched") {
    v <- sum(D[seq_len(n)]^2) / n
    return(list(var = v / n, info = list(unit = "observation", m = n)))
  }
  if (target == "PATE") {
    res <- y[seq_len(n)] - qa[seq_len(n)]
    rho <- 0.0
    for (grp in groups) {
      if (length(grp) > 1L) {
        for (a in seq_len(length(grp) - 1L)) {
          for (b in seq.int(a + 1L, length(grp))) {
            rho <- rho + res[grp[a]] * res[grp[b]]
          }
        }
      }
    }
    rho <- rho / length(groups)
    v <- sum(D[seq_len(n)]^2) / n - 2.0 * rho
    return(list(
      var = max(v, 0.0) / n,
      info = list(unit = "pair", m = length(groups), rho = rho)
    ))
  }
  dbar <- vapply(groups, function(grp) sum(D[grp]) / length(grp), numeric(1))
  m <- length(groups)
  v <- sum(dbar^2) / m
  list(var = v / m, info = list(unit = "pair", m = m))
}

#' Eq. (13.5)/(13.6) unmatched, (13.8)/(13.9) matched
#'
#' A step of the tmlcic_native implementation. Called by
#' \code{morie_tmlcic_adaptive_prespecification}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param D A vector; indexed elementwise.
#' @param y A vector; indexed elementwise.
#' @param qa A vector; indexed elementwise.
#' @param groups See Usage.
#' @param design Compared against \code{"unmatched"}.
#' @param target Compared against \code{"PATE"}.
#' @param rows A vector; its length is taken.
#' @return One of two values, depending on the branch taken.
#' @export
.tmlcic_loss <- function(D, y, qa, groups, design, target, rows) {
  # Eq. (13.5)/(13.6) unmatched, (13.8)/(13.9) matched.
  if (design == "unmatched") {
    return(sum(D[rows]^2) / length(rows))
  }
  tot <- 0.0
  m <- 0L
  for (grp in groups) {
    g <- intersect(grp, rows)
    if (length(g) == 0L) {
      next
    }
    m <- m + 1L
    if (target == "PATE") {
      val <- sum(D[g]^2) / length(g)
      if (length(g) > 1L) {
        for (a in seq_len(length(g) - 1L)) {
          for (b in seq.int(a + 1L, length(g))) {
            val <- val - 2.0 * (y[g[a]] - qa[g[a]]) * (y[g[b]] - qa[g[b]])
          }
        }
      }
      tot <- tot + val
    } else {
      dbar <- sum(D[g]) / length(g)
      tot <- tot + dbar * dbar
    }
  }
  if (m > 0L) tot / m else Inf
}

#' Folds that respect the pairing: a pair is never split
#'
#' A step of the tmlcic_native implementation. Called by
#' \code{morie_tmlcic_adaptive_prespecification}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param groups See Usage.
#' @param n_folds Optional; may be \code{NULL}. Coerced to integer by the body, with
#' \code{as.integer}.
#' @param design Compared against \code{"unmatched"}.
#' @param n A count; the body uses it as \code{seq_len(...)}.
#' @return The value of \code{Filter}.
#' @export
.tmlcic_cv_folds <- function(groups, n_folds, design, n) {
  # Folds that respect the pairing: a pair is never split.
  if (design == "unmatched") {
    units <- lapply(seq_len(n), function(i) i)
  } else {
    units <- groups
  }
  V <- if (is.null(n_folds) || identical(n_folds, 0) ||
    identical(n_folds, "loo")) {
    length(units)
  } else {
    max(2L, min(as.integer(n_folds), length(units)))
  }
  folds <- vector("list", V)
  for (j in seq_along(units)) {
    slot <- ((j - 1L) %% V) + 1L
    folds[[slot]] <- c(folds[[slot]], units[[j]])
  }
  Filter(function(f) length(f) > 0L, folds)
}

#' morie_tmlcic_adaptive_prespecification
#'
#' A step of the tmlcic_native implementation. Called by \code{morie_tmlcic_tmle_cluster_ic}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y A vector; its length is taken.
#' @param A Passed to \code{morie_tmlcic_candidate_tmle}.
#' @param W A matrix; passed to \code{ncol}.
#' @param groups Passed to \code{.tmlcic_cv_folds}.
#' @param design Passed to \code{.tmlcic_cv_folds}.
#' @param target Passed to \code{morie_tmlcic_influence_curve}.
#' @param library Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @param g_library Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @param n_folds Passed to \code{.tmlcic_cv_folds}.
#' @param ridge Passed to \code{morie_tmlcic_candidate_tmle}. Defaults to \code{1e-08}.
#' @return A list with \code{q_candidate}, \code{q_risks}, \code{q_names},
#' \code{g_candidate}, \code{g_risks}, \code{g_names}, \code{gfit}, \code{n_folds}.
#' @export
morie_tmlcic_adaptive_prespecification <- function(y, A, W, groups, design,
                                                   target, library = NULL,
                                                   g_library = NULL,
                                                   n_folds = NULL, ridge = 1e-8) {
  # Sec. 13.2-13.4: choose the working model, then choose g.
  n <- length(y)
  p <- ncol(W)
  lib <- if (is.null(library)) morie_tmlcic_default_library(p) else library
  folds <- .tmlcic_cv_folds(groups, n_folds, design, n)
  known_g <- function(i) 0.5
  cv_risk <- function(cand, gfit) {
    tot <- 0.0
    for (val in folds) {
      train <- setdiff(seq_len(n), val)
      if (length(train) == 0L) {
        next
      }
      g1 <- gfit(train)
      ct <- morie_tmlcic_candidate_tmle(y, A, W, cand, g1,
        rows = train,
        ridge = ridge
      )
      psi <- sum(ct$q1[val] - ct$q0[val]) / length(val)
      D <- morie_tmlcic_influence_curve(
        y, A, ct$q1, ct$q0, ct$qa,
        ct$info$gA, val, psi, target
      )
      tot <- tot + .tmlcic_loss(D, y, ct$qa, groups, design, target, val)
    }
    tot / length(folds)
  }
  q_risks <- vapply(
    lib, function(c) cv_risk(c, function(t) known_g),
    numeric(1)
  )
  best_q <- which.min(q_risks)
  chosen <- lib[[best_q]]
  glib <- if (is.null(g_library)) {
    g <- list(list(name = "known (0.5)", cols = integer(0), interact = FALSE))
    for (j in seq_len(p)) {
      g <- c(g, list(list(
        name = sprintf("W%d", j), cols = (j - 1L),
        interact = FALSE
      )))
    }
    g
  } else {
    g_library
  }
  gfit_for <- function(cand_g) {
    if (identical(cand_g$name, "known (0.5)") && length(cand_g$cols) == 0L) {
      function(t) known_g
    } else {
      function(train) .tmlcic_fit_g(A, W, cand_g, train, ridge)$g1
    }
  }
  g_risks <- vapply(
    glib, function(cg) cv_risk(chosen, gfit_for(cg)),
    numeric(1)
  )
  best_g <- which.min(g_risks)
  list(
    q_candidate = chosen, q_risks = q_risks,
    q_names = vapply(lib, function(c) c$name, character(1)),
    g_candidate = glib[[best_g]], g_risks = g_risks,
    g_names = vapply(glib, function(c) c$name, character(1)),
    gfit = gfit_for(glib[[best_g]]), n_folds = length(folds)
  )
}

#' morie_tmlcic_tmle_cluster_ic
#'
#' A step of the tmlcic_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Passed to \code{.s03vec}.
#' @param D Passed to \code{.s03vec}.
#' @param X Optional; may be \code{NULL}. Passed to \code{.s03mat}.
#' @param cluster Optional; may be \code{NULL}. Passed to \code{.tmlcic_pairs_from}.
#' @param target Passed to \code{morie_tmlcic_adaptive_prespecification}. Defaults to \code{"SATE"}.
#' @param design Optional; may be \code{NULL}. One of \code{"matched"}, \code{"unmatched"}.
#' @param library Passed to \code{morie_tmlcic_adaptive_prespecification}.
#' @param g_library Passed to \code{morie_tmlcic_adaptive_prespecification}.
#' @param n_folds Passed to \code{morie_tmlcic_adaptive_prespecification}.
#' @param adapt A flag; the body branches on it. Defaults to \code{TRUE}.
#' @param ridge Passed to \code{morie_tmlcic_adaptive_prespecification}. Defaults to \code{1e-08}.
#' @param level Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.95}.
#' @return A list with \code{estimate}, \code{se}, \code{n}, \code{ci}, \code{level},
#' \code{unadjusted}, \code{se_unadjusted}, \code{variance_ratio}, \code{q_selected},
#' \code{q_risks}, \code{g_selected}, \code{g_risks}, \code{epsilon},
#' \code{influence_curve}, \code{eic_mean}, \code{rho}, \code{independent_units},
#' \code{unit}, \code{design}, \code{target}, \code{n_folds}, \code{adapt},
#' \code{method}.
#' @export
morie_tmlcic_tmle_cluster_ic <- function(y, D, X, cluster = NULL,
                                         target = "SATE", design = NULL,
                                         library = NULL, g_library = NULL,
                                         n_folds = NULL, adapt = TRUE,
                                         ridge = 1e-8, level = 0.95) {
  # TMLE for a cluster randomized trial, with adaptive
  # pre-specification of the adjustment set.
  if (!(target %in% .tmlcic_TARGETS)) {
    stop(sprintf("tmlcic: target must be SATE or PATE, got %s", target))
  }
  yv <- .s03vec(y)
  Av <- .s03vec(D)
  n <- length(yv)
  if (length(Av) != n) {
    stop(sprintf("tmlcic: %d outcomes but %d treatments", n, length(Av)))
  }
  if (any(!(Av %in% c(0.0, 1.0)))) {
    stop("tmlcic: the randomization indicator must be binary 0/1")
  }
  if (!(sum(Av) > 0 && sum(Av) < n)) {
    stop("tmlcic: both arms must be non-empty")
  }
  Wm <- if (!is.null(X)) .s03mat(X) else matrix(numeric(0), n, 0)
  if (nrow(Wm) != n) {
    stop(sprintf("tmlcic: %d covariate rows for %d outcomes", nrow(Wm), n))
  }
  if (is.null(design)) {
    design <- if (is.null(cluster)) "unmatched" else "matched"
  }
  if (!(design %in% .tmlcic_DESIGNS)) {
    stop(sprintf(
      "tmlcic: design must be one of %s, got %s",
      paste(.tmlcic_DESIGNS, collapse = ", "), design
    ))
  }
  groups <- if (design != "unmatched") {
    .tmlcic_pairs_from(cluster, n)
  } else {
    lapply(seq_len(n), function(i) i)
  }
  if (design == "matched" && any(vapply(groups, length, integer(1)) != 2L)) {
    stop(paste0(
      "tmlcic: design='matched' needs pairs; use ",
      "design='clustered' for other sizes"
    ))
  }
  if (n < 4L) {
    stop(sprintf("tmlcic: need at least 4 units, got %d", n))
  }
  ymin <- min(yv)
  ymax <- max(yv)
  rng <- ymax - ymin
  if (rng <= 0.0) {
    stop("tmlcic: the outcome is constant")
  }
  ys <- (yv - ymin) / rng
  unadj <- list(name = "unadjusted", cols = integer(0), interact = FALSE)
  known_g <- function(i) 0.5
  if (adapt) {
    sel <- morie_tmlcic_adaptive_prespecification(ys, Av, Wm, groups,
      design, target,
      library = library,
      g_library = g_library,
      n_folds = n_folds,
      ridge = ridge
    )
    cand <- sel$q_candidate
    gfit <- sel$gfit
    g1 <- gfit(seq_len(n))
  } else {
    sel <- list(
      q_candidate = unadj, q_risks = numeric(0), q_names = character(0),
      g_candidate = list(name = "known (0.5)"), g_risks = numeric(0),
      g_names = character(0), n_folds = 0L
    )
    cand <- unadj
    g1 <- known_g
  }
  ct <- morie_tmlcic_candidate_tmle(ys, Av, Wm, cand, g1, ridge = ridge)
  rows <- seq_len(n)
  psi_s <- sum(ct$q1 - ct$q0) / n
  Dic <- morie_tmlcic_influence_curve(
    ys, Av, ct$q1, ct$q0, ct$qa,
    ct$info$gA, rows, psi_s, target
  )
  ve <- morie_tmlcic_variance_estimate(
    Dic, ys, ct$qa, groups, n, design,
    target
  )
  var_s <- ve$var
  vinfo <- ve$info
  cu <- morie_tmlcic_candidate_tmle(ys, Av, Wm, unadj, known_g, ridge = ridge)
  psi_u <- sum(cu$q1 - cu$q0) / n
  Du <- morie_tmlcic_influence_curve(
    ys, Av, cu$q1, cu$q0, cu$qa,
    cu$info$gA, rows, psi_u, target
  )
  var_u <- morie_tmlcic_variance_estimate(
    Du, ys, cu$qa, groups, n, design,
    target
  )$var
  psi <- rng * psi_s
  se <- rng * sqrt(var_s)
  se_u <- rng * sqrt(var_u)
  z <- .s03qnorm(0.5 + 0.5 * as.numeric(level))
  list(
    estimate = psi, se = se, n = n,
    ci = c(psi - z * se, psi + z * se), level = as.numeric(level),
    unadjusted = rng * psi_u, se_unadjusted = se_u,
    variance_ratio = if (var_u > 0.0) var_s / var_u else NaN,
    q_selected = sel$q_candidate$name,
    q_risks = stats::setNames(as.list(sel$q_risks), sel$q_names),
    g_selected = sel$g_candidate$name,
    g_risks = stats::setNames(as.list(sel$g_risks), sel$g_names),
    epsilon = ct$info$eps,
    influence_curve = Dic[rows] * rng,
    eic_mean = sum(Dic[rows]) / n,
    rho = if (!is.null(vinfo$rho)) vinfo$rho else NaN,
    independent_units = vinfo$m, unit = vinfo$unit,
    design = design, target = target, n_folds = sel$n_folds, adapt = isTRUE(adapt),
    method = paste0(
      "adaptive pre-specification TMLE, Balzer, van der ",
      "Laan & Petersen (2018) Ch. 13"
    )
  )
}

# ---------------------------------------------------------------------
# Hierarchical data: the cluster-level and individual-level TMLEs
# ---------------------------------------------------------------------

#' The per-individual weights alpha_ij and the cluster groups. Balzer
#'
#' et al. (2019) require sum_i alpha_ij = 1 within each cluster. The
#' default alpha_ij = 1/N_j is their stated choice.
#'
#' @param cluster Coerced to character by the body, with \code{as.character}.
#' @param weights Optional; may be \code{NULL}. Coerced to numeric by the body, with
#' \code{as.numeric}.
#' @return A list with \code{alpha}, \code{groups}.
#' @export
morie_tmlcic_cluster_weights <- function(cluster, weights = NULL) {
  # The per-individual weights alpha_ij and the cluster groups. Balzer
  # et al. (2019) require sum_i alpha_ij = 1 within each cluster. The
  # default alpha_ij = 1/N_j is their stated choice.
  lab <- as.character(cluster)
  n <- length(lab)
  order_ <- character(0)
  groups <- list()
  for (i in seq_len(n)) {
    c <- lab[i]
    if (is.null(groups[[c]])) {
      groups[[c]] <- integer(0)
      order_ <- c(order_, c)
    }
    groups[[c]] <- c(groups[[c]], i)
  }
  grp <- lapply(order_, function(c) groups[[c]])
  if (is.null(weights)) {
    alpha <- numeric(n)
    for (g in grp) {
      alpha[g] <- 1.0 / length(g)
    }
    return(list(alpha = alpha, groups = grp))
  }
  alpha <- as.numeric(weights)
  if (length(alpha) != n) {
    stop(sprintf("cluster_weights: %d weights for %d rows", length(alpha), n))
  }
  if (any(alpha < 0.0)) {
    stop("cluster_weights: weights must be non-negative")
  }
  for (g in grp) {
    tot <- sum(alpha[g])
    if (abs(tot - 1.0) > 1e-8) {
      stop(sprintf(
        "cluster_weights: weights in a cluster sum to %.6f, not 1",
        tot
      ))
    }
  }
  list(alpha = alpha, groups = grp)
}

#' Pull a cluster-level variable out of per-individual rows
#'
#' A step of the tmlcic_native implementation. Called by \code{morie_tmlcic_tmle_hierarchical}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v A vector; indexed elementwise.
#' @param groups A vector; its length is taken and its elements indexed.
#' @param name Passed to \code{sprintf}.
#' @return The value of \code{out}, as built in the body.
#' @export
.tmlcic_one_per_cluster <- function(v, groups, name) {
  # Pull a cluster-level variable out of per-individual rows.
  out <- numeric(length(groups))
  for (t in seq_along(groups)) {
    g <- groups[[t]]
    first <- v[g[1]]
    if (any(v[g] != first)) {
      stop(sprintf(
        "tmlcic: %s varies within a cluster; it is a %s", name,
        "cluster-level variable"
      ))
    }
    out[t] <- first
  }
  out
}

#' .tmlcic_hier_cluster_arm
#'
#' A step of the tmlcic_native implementation. Called by \code{morie_tmlcic_tmle_hierarchical}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param yc Numeric; combined arithmetically in the body.
#' @param Aj A vector; indexed elementwise.
#' @param Zj A matrix; passed to \code{as.matrix}.
#' @param groups A vector; its length is taken.
#' @param a Passed to \code{==}.
#' @param trim Numeric; combined arithmetically in the body.
#' @param ridge Numeric; passed to \code{max}.
#' @param known_g Optional; may be \code{NULL}. Coerced to numeric by the body, with
#' \code{as.numeric}.
#' @return A list with \code{psi}, \code{D}, \code{info}.
#' @export
.tmlcic_hier_cluster_arm <- function(yc, Aj, Zj, groups, a, trim, ridge,
                                     known_g) {
  # TMLE I, eq. (4)-(9): fit, target and average at cluster level.
  J <- length(groups)
  Xg <- .s03design(Zj, J)
  if (!is.null(known_g)) {
    p1 <- pmin(pmax(as.numeric(known_g), trim), 1.0 - trim)
  } else {
    b <- .tmlcic_wlogit(Xg, Aj, ridge = max(ridge, 1e-10))
    p1 <- pmin(pmax(.tmlcic_sig(as.numeric(Xg %*% b)), trim), 1.0 - trim)
  }
  ga <- vapply(
    seq_len(J), function(j) if (a == 1.0) p1[j] else 1.0 - p1[j],
    numeric(1)
  )
  Zjm <- as.matrix(Zj)
  rowf <- function(av, j) c(1.0, av, Zjm[j, ])
  Xq <- do.call(rbind, lapply(seq_len(J), function(j) rowf(Aj[j], j)))
  bq <- .tmlcic_wlogit(Xq, yc, ridge = max(ridge, 1e-10))
  q <- function(av, j) .s03sigmoid(sum(bq * rowf(av, j)))
  H <- vapply(
    seq_len(J), function(j) if (Aj[j] == a) 1.0 / ga[j] else 0.0,
    numeric(1)
  )
  off <- vapply(seq_len(J), function(j) .tmlcic_logit(q(Aj[j], j)), numeric(1))
  eps <- .tmlcic_fluct(yc, off, H)
  qs_obs <- .tmlcic_sig(off + eps * H)
  qs_a <- vapply(
    seq_len(J),
    function(j) .s03sigmoid(.tmlcic_logit(q(a, j)) + eps / ga[j]),
    numeric(1)
  )
  psi <- sum(qs_a) / J
  D <- H * (yc - qs_obs) + qs_a - psi
  list(psi = psi, D = D, info = list(
    eps = eps, max_weight = max(1.0 / ga),
    min_g = min(ga)
  ))
}

#' .tmlcic_hier_individual_arm
#'
#' A step of the tmlcic_native implementation. Called by \code{morie_tmlcic_tmle_hierarchical}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y A vector; its length is taken and its elements indexed.
#' @param Ai A vector; indexed elementwise.
#' @param Zi A matrix; passed to \code{as.matrix}.
#' @param alpha A vector; indexed elementwise.
#' @param groups A vector; its length is taken.
#' @param a Passed to \code{==}.
#' @param trim Numeric; combined arithmetically in the body.
#' @param ridge Numeric; passed to \code{max}.
#' @param known_g Optional; may be \code{NULL}. Coerced to numeric by the body, with
#' \code{as.numeric}.
#' @return A list with \code{psi}, \code{D}, \code{info}.
#' @export
.tmlcic_hier_individual_arm <- function(y, Ai, Zi, alpha, groups, a, trim,
                                        ridge, known_g) {
  # TMLE II, eq. (14)-(21): individual clever covariate, targeted
  # predictions averaged within cluster afterwards.
  n <- length(y)
  J <- length(groups)
  Xg <- .s03design(Zi, n)
  if (!is.null(known_g)) {
    p1 <- pmin(pmax(as.numeric(known_g), trim), 1.0 - trim)
  } else {
    b <- .tmlcic_wlogit(Xg, Ai, ridge = max(ridge, 1e-10), obs_weights = alpha)
    p1 <- pmin(pmax(.tmlcic_sig(as.numeric(Xg %*% b)), trim), 1.0 - trim)
  }
  ga <- vapply(
    seq_len(n), function(i) if (a == 1.0) p1[i] else 1.0 - p1[i],
    numeric(1)
  )
  Zim <- as.matrix(Zi)
  rowf <- function(av, i) c(1.0, av, Zim[i, ])
  Xq <- do.call(rbind, lapply(seq_len(n), function(i) rowf(Ai[i], i)))
  bq <- .tmlcic_wlogit(Xq, y, ridge = max(ridge, 1e-10), obs_weights = alpha)
  q <- function(av, i) .s03sigmoid(sum(bq * rowf(av, i)))
  H <- vapply(
    seq_len(n), function(i) if (Ai[i] == a) 1.0 / ga[i] else 0.0,
    numeric(1)
  )
  off <- vapply(seq_len(n), function(i) .tmlcic_logit(q(Ai[i], i)), numeric(1))
  eps <- .tmlcic_fluct(y, off, H, obs_weights = alpha)
  qs_obs <- .tmlcic_sig(off + eps * H)
  qs_a <- vapply(
    seq_len(n),
    function(i) .s03sigmoid(.tmlcic_logit(q(a, i)) + eps / ga[i]),
    numeric(1)
  )
  qc_a <- vapply(groups, function(g) sum(alpha[g] * qs_a[g]), numeric(1))
  psi <- sum(qc_a) / J
  D <- vapply(
    groups,
    function(g) sum(alpha[g] * (H[g] * (y[g] - qs_obs[g]) + qs_a[g])),
    numeric(1)
  ) - psi
  list(psi = psi, D = D, info = list(
    eps = eps, max_weight = max(1.0 / ga),
    min_g = min(ga), qc = qc_a
  ))
}

#' morie_tmlcic_tmle_hierarchical
#'
#' A step of the tmlcic_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Passed to \code{.s03vec}.
#' @param A Passed to \code{.s03vec}.
#' @param E Optional; may be \code{NULL}. Passed to \code{.s03mat}.
#' @param W Optional; may be \code{NULL}. Passed to \code{.s03mat}.
#' @param cluster Passed to \code{morie_tmlcic_cluster_weights}.
#' @param arm One of \code{"both"}, \code{"cluster"}, \code{"individual"}. Defaults to
#' \code{"both"}.
#' @param weights Passed to \code{morie_tmlcic_cluster_weights}.
#' @param known_g Optional; may be \code{NULL}. A vector; indexed elementwise.
#' @param trim Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.01}.
#' @param ridge Passed to \code{.tmlcic_hier_cluster_arm}. Defaults to \code{1e-08}.
#' @param level Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.95}.
#' @return The value of \code{payload}, as built in the body.
#' @export
morie_tmlcic_tmle_hierarchical <- function(y, A, E, W, cluster, arm = "both",
                                           weights = NULL, known_g = NULL,
                                           trim = 0.01, ridge = 1e-8,
                                           level = 0.95) {
  # Causal effect of a CLUSTER-level exposure on hierarchical data: two
  # estimators of E[Yc(1)] - E[Yc(0)], TMLE I (cluster) and TMLE II
  # (individual). Both are returned.
  if (!(arm %in% c("both", "cluster", "individual"))) {
    stop(sprintf(
      "tmlcic: arm must be both, cluster or individual, got %s",
      arm
    ))
  }
  yv <- .s03vec(y)
  Av <- .s03vec(A)
  n <- length(yv)
  if (length(Av) != n) {
    stop(sprintf("tmlcic: %d outcomes but %d exposures", n, length(Av)))
  }
  if (any(!(Av %in% c(0.0, 1.0)))) {
    stop("tmlcic: the exposure must be binary 0/1")
  }
  if (any(yv < 0.0 | yv > 1.0)) {
    stop(paste0(
      "tmlcic: individual outcomes must lie in [0, 1]; rescale ",
      "them first"
    ))
  }
  Em <- if (!is.null(E)) .s03mat(E) else matrix(numeric(0), n, 0)
  Wm <- if (!is.null(W)) .s03mat(W) else matrix(numeric(0), n, 0)
  if (nrow(Em) != n || nrow(Wm) != n) {
    stop(sprintf(
      "tmlcic: covariate blocks have %d and %d rows for %d %s",
      nrow(Em), nrow(Wm), n, "individuals"
    ))
  }
  t <- as.numeric(trim)
  if (!(t > 0.0 && t < 0.5)) {
    stop(sprintf("tmlcic: trim must be in (0, 0.5), got %g", trim))
  }
  cw <- morie_tmlcic_cluster_weights(cluster, weights)
  alpha <- cw$alpha
  groups <- cw$groups
  J <- length(groups)
  if (J < 4L) {
    stop(sprintf("tmlcic: need at least 4 clusters, got %d", J))
  }
  Aj <- .tmlcic_one_per_cluster(Av, groups, "the exposure")
  if (!(sum(Aj) > 0 && sum(Aj) < J)) {
    stop("tmlcic: both exposure arms must be non-empty")
  }
  if (ncol(Em) > 0L) {
    for (c in seq_len(ncol(Em))) {
      .tmlcic_one_per_cluster(Em[, c], groups, "a cluster-level covariate")
    }
  }
  yc <- vapply(groups, function(g) sum(alpha[g] * yv[g]), numeric(1))
  Ej <- do.call(rbind, lapply(groups, function(g) Em[g[1], , drop = FALSE]))
  nWc <- ncol(Wm)
  Wbar <- do.call(rbind, lapply(groups, function(g) {
    if (nWc > 0L) {
      vapply(seq_len(nWc), function(c) sum(alpha[g] * Wm[g, c]), numeric(1))
    } else {
      numeric(0)
    }
  }))
  Zj <- cbind(Ej, Wbar)
  Zi <- cbind(Em, Wm)
  kg_c <- if (!is.null(known_g)) known_g[[1]] else NULL
  kg_i <- if (!is.null(known_g)) known_g[[2]] else NULL
  out <- list()
  z <- .s03qnorm(0.5 + 0.5 * as.numeric(level))
  runs <- list(
    cluster = (arm %in% c("both", "cluster")),
    individual = (arm %in% c("both", "individual"))
  )
  for (nm in names(runs)) {
    if (!runs[[nm]]) {
      next
    }
    psi <- list()
    D <- list()
    info <- list()
    for (a in c(0.0, 1.0)) {
      ak <- as.character(a)
      if (nm == "cluster") {
        r <- .tmlcic_hier_cluster_arm(yc, Aj, Zj, groups, a, t, ridge, kg_c)
      } else {
        r <- .tmlcic_hier_individual_arm(
          yv, Av, Zi, alpha, groups, a, t,
          ridge, kg_i
        )
      }
      psi[[ak]] <- r$psi
      D[[ak]] <- r$D
      info[[ak]] <- r$info
    }
    contrast <- psi[["1"]] - psi[["0"]]
    Dc <- D[["1"]] - D[["0"]]
    se <- if (J > 1L) .s03sd(Dc) / sqrt(J) else NaN
    out[[nm]] <- list(
      estimate = contrast, se = se,
      ci = c(contrast - z * se, contrast + z * se),
      mean_1 = psi[["1"]], mean_0 = psi[["0"]],
      influence_curve = Dc, eic_mean = sum(Dc) / J,
      epsilon = c(info[["0"]]$eps, info[["1"]]$eps),
      max_weight = max(
        info[["0"]]$max_weight,
        info[["1"]]$max_weight
      )
    )
  }
  main <- if (!is.null(out[["individual"]])) "individual" else "cluster"
  payload <- list(
    estimate = out[[main]]$estimate, se = out[[main]]$se, ci = out[[main]]$ci,
    arm_reported = main, arm = arm, n = n, n_clusters = J,
    cluster_sizes = vapply(groups, length, integer(1)),
    cluster_outcome = yc, alpha = alpha, level = as.numeric(level),
    known_g = !is.null(known_g),
    method = paste0(
      "hierarchical TMLE for a cluster-level exposure, ",
      "Balzer, Zheng, van der Laan & Petersen (2019)"
    )
  )
  for (nm in names(out)) {
    for (key in names(out[[nm]])) {
      payload[[sprintf("%s_%s", key, nm)]] <- out[[nm]][[key]]
    }
  }
  payload
}

#' morie_tmlcic_cheatsheet
#'
#' A step of the tmlcic_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
morie_tmlcic_cheatsheet <- function() {
  paste0(
    "tmlcic: cluster randomized trial. Pre-specify a LIBRARY of ",
    "working models, select by cross-validated squared ",
    "influence curve (the TMLE's own variance). Losses: 13.5 ",
    "PATE / 13.6 SATE unmatched, 13.8 / 13.9 matched -- the ",
    "matched ones subtract the within-pair residual covariance ",
    "so a perfectly matched covariate earns no credit. Then ",
    "select g collaboratively by the same loss. Pairs are never ",
    "split across folds."
  )
}

# compact alias per ledger/NAMING.md
morie_tmlcic_tmleclusteric <- morie_tmlcic_tmle_cluster_ic

#' @export
morie_tmlcic <- morie_tmlcic_tmle_cluster_ic
