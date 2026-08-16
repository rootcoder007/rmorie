# bhltmsm -- Marginal structural model for cumulative treatment episodes.
# Griffin et al. (2014); Robins, Hernan & Brumback (2000).
# Base R only.

.bhltmsm_EPS <- 1e-12
.STATES <- c("none", "outpatient", "residential", "screening")

#' cumulative_episodes
#'
#' Part of the bhltmsm_native implementation; see the file header for
#' the source it follows.
#'
#' @param histories See Usage.
#' @param states Defaults to \code{.STATES}.
#' @return A list with \code{cumulative}, \code{states}, \code{periods}, \code{note}.
#' @export
cumulative_episodes <- function(histories, states = .STATES) {
  S <- as.character(states)
  idx <- setNames(seq_along(S), S)
  out <- matrix(0, nrow = length(histories), ncol = length(S))
  for (i in seq_along(histories)) {
    h <- histories[[i]]
    for (a in h) {
      key <- if (is.character(a)) a else S[as.integer(a) + 1L]
      if (is.na(idx[key]))
        stop(sprintf("bhltmsm: unknown treatment state %s; the states are %s",
                     key, paste(S, collapse = ", ")))
      out[i, idx[key]] <- out[i, idx[key]] + 1
    }
  }
  list(cumulative = out, states = S, periods = rowSums(out),
       note = paste("mutually exclusive states per period, so the",
                    "counts add to the number of periods"))
}

.quantile7 <- function(x, q) {
  # Linear interpolation, type 7 (R default)
  x <- sort(as.numeric(x))
  if (length(x) == 0) return(NA_real_)
  if (length(x) == 1) return(x)
  h <- (length(x) - 1) * q
  lo <- floor(h); hi <- ceiling(h)
  if (lo == hi) return(x[lo + 1L])
  x[lo + 1L] + (h - lo) * (x[hi + 1L] - x[lo + 1L])
}

#' treatment_weights
#'
#' Part of the bhltmsm_native implementation; see the file header for
#' the source it follows.
#'
#' @param histories See Usage.
#' @param propensities See Usage.
#' @param stabilise Defaults to \code{TRUE}.
#' @param marginal Defaults to \code{NULL}.
#' @param truncate Defaults to \code{NULL}.
#' @return A list with \code{weights}, \code{raw}, \code{stabilised}, \code{truncated}, \code{n_truncated}, \code{note}.
#' @export
treatment_weights <- function(histories, propensities, stabilise = TRUE,
                              marginal = NULL, truncate = NULL) {
  W <- numeric(length(histories))
  raw <- numeric(length(histories))
  for (i in seq_along(histories)) {
    h <- histories[[i]]
    P <- as.numeric(propensities[[i]])
    if (length(P) != length(h))
      stop(sprintf("bhltmsm: person %d has %d periods but %d propensities",
                   i, length(h), length(P)))
    if (any(P <= 0 | P > 1))
      stop(sprintf(paste("bhltmsm: a propensity is outside (0,1] for",
                         "person %d -- positivity fails, so the weight",
                         "is undefined"), i))
    den <- 1
    for (v in P) den <- den * v
    num <- 1
    if (stabilise) {
      if (is.null(marginal))
        stop("bhltmsm: stabilised weights need the marginal probabilities")
      Q <- as.numeric(marginal[[i]])
      if (length(Q) != length(P))
        stop("bhltmsm: the marginal model has a different number of periods")
      for (v in Q) num <- num * v
    }
    w <- num / den
    raw[i] <- w
    W[i] <- w
  }
  n_trunc <- 0L
  if (!is.null(truncate)) {
    lo <- .quantile7(raw, as.numeric(truncate))
    hi <- .quantile7(raw, 1 - as.numeric(truncate))
    W2 <- pmin(pmax(W, lo), hi)
    n_trunc <- sum(W2 != W)
    W <- W2
  }
  list(weights = W, raw = raw, stabilised = as.logical(stabilise),
       truncated = !is.null(truncate), n_truncated = n_trunc,
       note = paste("truncation trades variance for BIAS, so what it",
                    "changed is reported"))
}

#' weight_diagnostics
#'
#' Part of the bhltmsm_native implementation; see the file header for
#' the source it follows.
#'
#' @param weights See Usage.
#' @return A list with \code{mean}, \code{max}, \code{min}, \code{effective_n}, \code{n}, \code{efficiency}, \code{mean_near_one}, \code{note}.
#' @export
weight_diagnostics <- function(weights) {
  w <- as.numeric(weights)
  n <- length(w)
  if (n < 2) stop("bhltmsm: at least 2 weights are needed")
  m <- mean(w)
  ess <- sum(w)^2 / sum(w^2)
  list(mean = m, max = max(w), min = min(w), effective_n = ess, n = n,
       efficiency = ess / n, mean_near_one = abs(m - 1) < 0.1,
       note = paste("effective sample size collapses when a few",
                    "weights dominate, which is what a positivity",
                    "violation looks like"))
}

.corr_r <- function(x, y) {
  x <- as.numeric(x); y <- as.numeric(y)
  n <- length(x)
  if (length(y) != n) stop("bhltmsm: length mismatch in corr")
  mx <- mean(x); my <- mean(y)
  num <- sum((x - mx) * (y - my))
  den <- sqrt(sum((x - mx)^2) * sum((y - my)^2))
  if (den == 0) return(0)
  num / den
}

#' confounding_check
#'
#' Part of the bhltmsm_native implementation; see the file header for
#' the source it follows.
#'
#' @param covariate_history See Usage.
#' @param treatment_history See Usage.
#' @param outcome Defaults to \code{NULL}.
#' @return The value of \code{out}, as built in the body.
#' @export
confounding_check <- function(covariate_history, treatment_history,
                              outcome = NULL) {
  L <- as.matrix(covariate_history); storage.mode(L) <- "double"
  A <- as.matrix(treatment_history); storage.mode(A) <- "double"
  if (nrow(L) != nrow(A))
    stop(sprintf("bhltmsm: %d covariate histories but %d treatment histories",
                 nrow(L), nrow(A)))
  Tn <- ncol(L)
  if (Tn < 2)
    stop("bhltmsm: at least 2 periods are needed to ask whether treatment affects the covariate")
  prior_to_l <- .corr_r(A[, 1], L[, 2])
  l_to_next_a <- .corr_r(L[, 1], A[, 2])
  both <- abs(prior_to_l) > 0.1 && abs(l_to_next_a) > 0.1
  out <- list(treatment_affects_covariate = prior_to_l,
              covariate_predicts_treatment = l_to_next_a,
              is_treatment_confounder_feedback = both,
              note = paste("both arrows present means neither adjusting",
                           "nor not adjusting is valid -- hence IPTW"))
  if (!is.null(outcome)) {
    y <- as.numeric(outcome)
    out$covariate_predicts_outcome <- .corr_r(L[, 2], y)
  }
  out
}

.wls_r <- function(X, y, w) {
  X <- as.matrix(X); storage.mode(X) <- "double"
  y <- as.numeric(y); w <- as.numeric(w)
  X1 <- cbind(1, X)
  storage.mode(X1) <- "double"
  WX <- X1 * w
  xtwx <- crossprod(WX, X1)
  xtwy <- crossprod(WX, y)
  solve(xtwx, xtwy)
}

#' fit_msm
#'
#' Part of the bhltmsm_native implementation; see the file header for
#' the source it follows.
#'
#' @param outcome See Usage.
#' @param cumulative See Usage.
#' @param weights Defaults to \code{NULL}.
#' @param states Defaults to \code{.STATES}.
#' @return A list with \code{estimate}, \code{intercept}, \code{coefficients}, \code{se}, \code{per_episode}, \code{weighted}, \code{effective_n}, \code{method}, \code{note}.
#' @export
fit_msm <- function(outcome, cumulative, weights = NULL, states = .STATES) {
  y <- as.numeric(outcome)
  X <- as.matrix(cumulative); storage.mode(X) <- "double"
  n <- length(y)
  if (nrow(X) != n)
    stop(sprintf("bhltmsm: %d outcomes but %d covariate rows", n, nrow(X)))
  w <- if (is.null(weights)) rep(1, n) else as.numeric(weights)
  if (length(w) != n)
    stop(sprintf("bhltmsm: %d weights for %d people", length(w), n))
  if (any(w < 0)) stop("bhltmsm: a weight is negative")
  co <- .wls_r(X, y, w)
  fit <- as.numeric(cbind(1, X) %*% co)
  res <- y - fit
  dof <- max(n - ncol(X) - 1, 1)
  s2 <- sum(w * res^2) / dof
  ses <- numeric(ncol(X))
  for (a in seq_len(ncol(X))) {
    xm <- sum(w * X[, a]) / sum(w)
    sxx <- sum(w * (X[, a] - xm)^2)
    ses[a] <- if (sxx > .bhltmsm_EPS) sqrt(s2 / sxx) else Inf
  }
  names <- as.character(states)[seq_len(ncol(X))]
  list(estimate = co[-1], intercept = co[1],
       coefficients = setNames(as.list(co[-1]), names),
       se = setNames(as.list(ses), names),
       per_episode = setNames(as.list(co[-1]), names),
       weighted = !is.null(weights),
       effective_n = sum(w)^2 / sum(w^2),
       method = paste("marginal structural model by IPTW;",
                      "Robins, Hernan & Brumback (2000), applied as in",
                      "Griffin et al. (2014)"),
       note = paste("the time-varying covariates are NOT regressors",
                    "here; they built the weights"))
}

behavioral_health_msm <- fit_msm

# house entry point: the package exports one morie_<module>
morie_bhltmsm <- fit_msm
