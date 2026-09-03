# morie.fn -- function file (rootcoder007/morie)
# Lagged-value IPTW.
#
# Robins (1986) introduced the g-formula and inverse-probability weighting
# for a *sustained exposure period*: treatment is repeated over time, and
# the covariates that predict the next treatment are themselves affected
# by the previous one. Section 6 of that paper is the origin of the
# weighted estimator that Hernan & Robins later present as Sec. 21.2.
#
# The specific thing this module implements is what makes the estimator
# usable in practice: the treatment model at time k conditions on the
# *lagged values* of the outcome and covariate processes, not just on
# their current values. Writing bar A_{k-1} for treatment history and
# bar L_k for covariate history, the denominator is
#
#   f(A_k | bar A_{k-1}, L_k, L_{k-1}, ..., L_{k-l}, Y_{k-1}, ..., Y_{k-l})
#
# for a lag l, and the weight is the product over k of the Sec. 21.2
# ratio. The stub this replaces said "include Y_{t-1} in propensity
# model" and then returned mean(y).
#
# Why the lag matters and is not decoration. The whole difficulty
# Robins (1986) identified is that a time-varying confounder affected by
# prior treatment cannot be handled by conditioning -- adjust for it and
# you block part of the treatment effect, omit it and you leave
# confounding. Weighting solves it, but only if the weight model captures
# the dependence, and with a sustained exposure that dependence reaches
# back more than one period. lag=0 reduces to the contemporaneous model,
# which is the common misuse; the anchor shows the two disagree.
#
# References
# ----------
# Robins, J. (1986) "A new approach to causal inference in mortality
# studies with a sustained exposure period -- application to control of
# the healthy worker survivor effect", Mathematical Modelling 7(9-12),
# 1393-1512, doi:10.1016/0270-0255(86)90088-6.
#
# Hernan, M. A. & Robins, J. M. (2020) Causal Inference: What If, Boca
# Raton: Chapman & Hall/CRC, Sec. 21.2 for the product-over-time weights
# and Ch. 20 for why conditioning fails where weighting works.


# ---- private helpers --------------------------------------------------

#' .lggvls_vec
#'
#' A step of the lggvls_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @return A vector, from \code{as.numeric}.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .lggvls_vec(x = x)
#' res
.lggvls_vec <- function(x) {
  if (is.null(x)) return(numeric(0))
  as.numeric(x)
}

#' .lggvls_mat
#'
#' A step of the lggvls_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.
#' @return A matrix, from \code{as.matrix}.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .lggvls_mat(x = x)
#' res
.lggvls_mat <- function(x) {
  if (is.null(x)) return(matrix(numeric(0), nrow = 0, ncol = 0))
  as.matrix(x)
}

# type-7 quantile (R default, matches numpy.quantile default)
#' Type-7 quantile (R default, matches numpy.quantile default)
#'
#' A step of the lggvls_native implementation. Called by \code{morie_lggvls}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x See Usage.
#' @param q See Usage.
#' @return A vector, from \code{as.numeric}.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .lggvls_quantile7(x = x, q = 0.5)
#' res
.lggvls_quantile7 <- function(x, q) {
  as.numeric(stats::quantile(x, probs = q, type = 7, names = FALSE))
}

# IRLS logistic regression (no package dependencies)
#' IRLS logistic regression (no package dependencies)
#'
#' A step of the lggvls_native implementation. Called by \code{.lggvls_ip_weights}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; passed to \code{nrow}.
#' @param y Numeric; combined arithmetically in the body.
#' @param max_iter A count; the body uses it as \code{seq_len(...)}. Defaults to \code{25L}.
#' @param tol Passed to \code{<}. Defaults to \code{1e-08}.
#' @return A list with \code{beta}, \code{p_hat}, \code{converged}.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' y <- c(2.9, 5.1, 6.8, 9.4, 11.2, 13.1, 15.0, 17.6)
#' res <- .lggvls_logistic_fit(X = x, y = y)
#' res
.lggvls_logistic_fit <- function(X, y, max_iter = 25L, tol = 1e-8) {
  n <- nrow(X)
  Xa <- cbind(1, X)
  p <- ncol(Xa)
  beta <- rep(0, p)
  for (iter in seq_len(max_iter)) {
    eta <- as.numeric(Xa %*% beta)
    eta <- pmin(pmax(eta, -30), 30)
    mu  <- 1 / (1 + exp(-eta))
    mu  <- pmin(pmax(mu, 1e-10), 1 - 1e-10)
    W   <- mu * (1 - mu)
    z   <- eta + (y - mu) / W
    WX  <- sweep(Xa, 1, W, "*")
    XtWX <- crossprod(Xa, WX)
    XtWz <- crossprod(Xa, W * z)
    beta_new <- tryCatch(solve(XtWX, XtWz), error = function(e) beta)
    if (max(abs(beta_new - beta)) < tol) { beta <- beta_new
    break }
    beta <- beta_new
  }
  eta <- as.numeric(Xa %*% beta)
  eta <- pmin(pmax(eta, -30), 30)
  p_hat <- 1 / (1 + exp(-eta))
  list(beta = as.numeric(beta), p_hat = as.numeric(p_hat), converged = TRUE)
}

# IPW for a single time point, binary treatment
#' IPW for a single time point, binary treatment
#'
#' A step of the lggvls_native implementation. Called by \code{morie_lggvls}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a A vector; its length is taken.
#' @param den_X Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.
#' @param num_X Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.
#' @param kind Compared against \code{"binary"}. Defaults to \code{"binary"}.
#' @param stabilize A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return A list with \code{w}, \code{info}.
#' @export
.lggvls_ip_weights <- function(a, den_X, num_X, kind = "binary", stabilize = TRUE) {
  n <- length(a)
  if (kind != "binary") {
    stop("only binary treatment is supported")
  }
  # denominator model
  if (is.null(den_X)) {
    p_den <- rep(mean(a), n)
    den_info <- list(type = "marginal", p = mean(a))
  } else {
    Xm <- as.matrix(den_X)
    if (ncol(Xm) == 0L) {
      p_den <- rep(mean(a), n)
      den_info <- list(type = "marginal", p = mean(a))
    } else {
      fit <- .lggvls_logistic_fit(Xm, a)
      p_den <- fit$p_hat
      den_info <- list(type = "logistic", beta = fit$beta)
    }
  }
  p_den <- pmin(pmax(p_den, 1e-10), 1 - 1e-10)
  # numerator model
  if (isTRUE(stabilize)) {
    if (is.null(num_X)) {
      p_num <- rep(mean(a), n)
      num_info <- list(type = "marginal", p = mean(a))
    } else {
      Xm <- as.matrix(num_X)
      if (ncol(Xm) == 0L) {
        p_num <- rep(mean(a), n)
        num_info <- list(type = "marginal", p = mean(a))
      } else {
        fit <- .lggvls_logistic_fit(Xm, a)
        p_num <- fit$p_hat
        num_info <- list(type = "logistic", beta = fit$beta)
      }
    }
    p_num <- pmin(pmax(p_num, 1e-10), 1 - 1e-10)
  } else {
    p_num <- rep(1, n)
    num_info <- list(type = "none")
  }
  # weights: P(A|A_past) / P(A|A_past, L, Y_lag)
  w1 <- p_num / p_den
  w0 <- (1 - p_num) / (1 - p_den)
  w  <- ifelse(a == 1, w1, w0)
  info <- list(denominator = den_info, numerator = num_info,
               kind = kind, stabilize = isTRUE(stabilize))
  list(w = as.numeric(w), info = info)
}

# weighted least squares, intercept added
#' Weighted least squares, intercept added
#'
#' A step of the lggvls_native implementation. Called by \code{morie_lggvls}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.
#' @param y A vector; its length is taken.
#' @param w Numeric; combined arithmetically in the body.
#' @return A list with \code{coef}, \code{se}, \code{vcov}, \code{sigma2}.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' y <- c(2.9, 5.1, 6.8, 9.4, 11.2, 13.1, 15.0, 17.6)
#' res <- .lggvls_wls(X = x, y = y, w = x)
#' res
.lggvls_wls <- function(X, y, w) {
  n <- length(y)
  if (is.null(X) || length(X) == 0L) {
    Xa <- matrix(1, nrow = n, ncol = 1L)
  } else {
    p <- length(X[[1]])
    Xm <- matrix(0, nrow = n, ncol = p)
    for (i in seq_len(n)) Xm[i, ] <- X[[i]]
    Xa <- cbind(1, Xm)
  }
  WX   <- sweep(Xa, 1, w, "*")
  XtWX <- crossprod(Xa, WX)
  XtWy <- crossprod(Xa, w * y)
  beta <- solve(XtWX, XtWy)
  resid <- y - as.numeric(Xa %*% beta)
  df <- n - ncol(Xa)
  sigma2 <- if (df > 0L) sum(w * resid^2) / df else 0
  vcov <- sigma2 * solve(XtWX)
  se <- sqrt(diag(vcov))
  list(coef = as.numeric(beta), se = as.numeric(se),
       vcov = vcov, sigma2 = sigma2)
}

# bind a list of length-n vectors into an n-by-p matrix
#' Bind a list of length-n vectors into an n-by-p matrix
#'
#' A step of the lggvls_native implementation. Called by \code{morie_lggvls}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param cols A vector; its length is taken.
#' @param n Accepted by the signature and not used anywhere in the body.
#' @return The value of \code{do.call}.
#' @export
.lggvls_bind <- function(cols, n) {
  if (length(cols) == 0L) return(NULL)
  do.call(cbind, cols)
}

# wrap a single vector/matrix as a one-element history
#' Wrap a single vector/matrix as a one-element history
#'
#' A step of the lggvls_native implementation. Called by \code{morie_lggvls}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param obj Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.
#' @param allow_none A flag; the body branches on it. Defaults to \code{FALSE}.
#' @return The value of \code{list}.
#' @export
.lggvls_as_history <- function(obj, allow_none = FALSE) {
  if (is.null(obj)) {
    if (isTRUE(allow_none)) return(list(NULL))
    return(list())
  }
  if (is.list(obj) && length(obj) > 0L) {
    first <- obj[[1]]
    if (is.null(first) || is.matrix(first) || is.numeric(first) ||
        is.data.frame(first)) {
      return(obj)
    }
  }
  list(obj)
}

# build columns at time k_time with `lag` earlier values
#' Build columns at time k_time with `lag` earlier values
#'
#' A step of the lggvls_native implementation. Called by \code{lagged_design}, \code{morie_lggvls}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param L_hist Optional; may be \code{NULL}. A vector; its length is taken and its
#' elements indexed.
#' @param Y_hist Optional; may be \code{NULL}. A vector; its length is taken and its
#' elements indexed.
#' @param k_time Numeric; combined arithmetically in the body. Defaults to \code{0}.
#' @param lag Numeric; combined arithmetically in the body. Defaults to \code{1}.
#' @return The value of \code{cols}, as built in the body.
#' @export
.lggvls_lagged_design <- function(L_hist, Y_hist = NULL, k_time = 0, lag = 1) {
  cols    <- list()
  k_time  <- as.integer(k_time)
  lag     <- as.integer(lag)
  lo      <- max(0L, k_time - lag)
  # L part: j = k_time, k_time-1, ..., lo
  if (!is.null(L_hist) && k_time >= lo) {
    for (j in k_time:lo) {
      rj <- j + 1L
      if (rj >= 1L && rj <= length(L_hist) && !is.null(L_hist[[rj]])) {
        block <- as.matrix(L_hist[[rj]])
        if (ncol(block) > 0L) {
          for (c in seq_len(ncol(block))) {
            cols[[length(cols) + 1L]] <- as.numeric(block[, c])
          }
        }
      }
    }
  }
  # Y part: j = k_time-1, ..., lo  (only if j >= 0, i.e. rj >= 1)
  if (!is.null(Y_hist) && (k_time - 1L) >= lo) {
    for (j in (k_time - 1L):lo) {
      rj <- j + 1L
      if (rj >= 1L && rj <= length(Y_hist) && !is.null(Y_hist[[rj]])) {
        cols[[length(cols) + 1L]] <- as.numeric(Y_hist[[rj]])
      }
    }
  }
  cols
}


# ---- public API -------------------------------------------------------

# Covariates at time `k_time` together with `lag` earlier values.
#' Covariates at time `k_time` together with `lag` earlier values
#'
#' A step of the lggvls_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param L_hist Passed to \code{.lggvls_lagged_design}.
#' @param Y_hist Passed to \code{.lggvls_lagged_design}.
#' @param k_time Passed to \code{.lggvls_lagged_design}. Defaults to \code{0}.
#' @param lag Passed to \code{.lggvls_lagged_design}. Defaults to \code{1}.
#' @return The value of \code{.lggvls_lagged_design}.
#' @export
lagged_design <- function(L_hist, Y_hist = NULL, k_time = 0, lag = 1) {
  .lggvls_lagged_design(L_hist, Y_hist, k_time, lag)
}

# IPTW for a sustained exposure, with lagged values in the treatment model.
#' IPTW for a sustained exposure, with lagged values in the treatment
#' model
#'
#' A step of the lggvls_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Coerced to numeric by the body, with \code{as.numeric}.
#' @param A Passed to \code{.lggvls_as_history}.
#' @param H Passed to \code{.lggvls_as_history}.
#' @param lag Passed to \code{.lggvls_lagged_design}. Defaults to \code{1}.
#' @param Y_hist Passed to \code{.lggvls_lagged_design}.
#' @param stabilize Passed to \code{.lggvls_ip_weights}. Defaults to \code{TRUE}.
#' @param kind Passed to \code{.lggvls_ip_weights}. Defaults to \code{"binary"}.
#' @param trim Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param contrast One of \code{"cumulative"}, \code{"everexposed"}, \code{"final"}.
#' Defaults to \code{"cumulative"}.
#' @return A list with \code{estimate}, \code{se}, \code{intercept}, \code{coef},
#' \code{vcov}, \code{weights}, \code{mean_weight}, \code{max_weight},
#' \code{effective_sample_size}, \code{cumulative_exposure}, \code{per_time},
#' \code{n_times}, \code{lag}, \code{n}, \code{contrast}, \code{method}.
#' @export
morie_lggvls <- function(y, A, H, lag = 1, Y_hist = NULL, stabilize = TRUE,
                        kind = "binary", trim = NULL,
                        contrast = "cumulative") {
  if (!contrast %in% c("cumulative", "final", "everexposed")) {
    stop(sprintf("laggedval_iptw: contrast must be 'cumulative', 'final' or 'everexposed', got %s",
                 deparse(contrast)))
  }
  lag <- as.integer(lag)
  if (lag < 0L) {
    stop(sprintf("laggedval_iptw: lag must be non-negative, got %d", lag))
  }

  A_hist <- .lggvls_as_history(A)
  K      <- length(A_hist)
  L_hist <- .lggvls_as_history(H, allow_none = TRUE)
  if (length(L_hist) < K) {
    L_hist <- c(L_hist, replicate(K - length(L_hist), NULL))
  }
  yv <- as.numeric(y)
  n  <- length(yv)

  for (kk in 0:(K - 1L)) {
    ak <- as.numeric(A_hist[[kk + 1L]])
    if (length(ak) != n) {
      stop(sprintf("laggedval_iptw: outcome has %d rows but treatment at time %d has %d",
                   n, kk, length(ak)))
    }
  }

  # Sec. 21.2's product, with the lagged design at each time point.
  w        <- rep(1.0, n)
  per_time <- list()
  past     <- list()
  for (kk in 0:(K - 1L)) {
    ak    <- as.numeric(A_hist[[kk + 1L]])
    cols  <- .lggvls_lagged_design(L_hist, Y_hist, kk, lag)
    den_X <- .lggvls_bind(c(cols, past), n)
    num_X <- if (length(past) > 0L) .lggvls_bind(past, n) else NULL
    res   <- .lggvls_ip_weights(ak, den_X, num_X, kind = kind, stabilize = stabilize)
    wk    <- res$w
    info  <- res$info
    w     <- w * wk
    per_time[[length(per_time) + 1L]] <- list(
      time         = kk,
      n_covariates = length(cols) + length(past),
      info         = info
    )
    past[[length(past) + 1L]] <- ak
  }

  if (!is.null(trim)) {
    q <- as.numeric(trim)
    if (!(0.5 < q && q < 1.0)) {
      stop("laggedval_iptw: trim must be in (0.5, 1)")
    }
    hi <- .lggvls_quantile7(w, q)
    lo <- .lggvls_quantile7(w, 1.0 - q)
    w  <- pmin(pmax(w, lo), hi)
  }

  cum <- numeric(n)
  for (kk in seq_len(K)) {
    cum <- cum + as.numeric(A_hist[[kk]])
  }

  if (contrast == "cumulative") {
    X <- lapply(seq_len(n), function(i) cum[i])
  } else if (contrast == "final") {
    last <- as.numeric(A_hist[[K]])
    X <- lapply(seq_len(n), function(i) last[i])
  } else {
    X <- lapply(seq_len(n), function(i) if (cum[i] > 0) 1.0 else 0.0)
  }

  fit <- .lggvls_wls(X, yv, w)
  s1  <- sum(w)
  s2  <- sum(w * w)

  list(
    estimate                = fit$coef[2],
    se                      = fit$se[2],
    intercept               = fit$coef[1],
    coef                    = fit$coef,
    vcov                    = fit$vcov,
    weights                 = w,
    mean_weight             = s1 / n,
    max_weight              = max(w),
    effective_sample_size   = if (s2 > 0.0) (s1 * s1) / s2 else 0.0,
    cumulative_exposure     = cum,
    per_time                = per_time,
    n_times                 = K,
    lag                     = lag,
    n                       = n,
    contrast                = contrast,
    method                  = paste("lagged-value IPTW, Robins (1986);",
                                    "weights by Hernan & Robins (2020) Sec. 21.2")
  )
}

# aliases matching the Python __all__ and the ledger naming note
laggedval_iptw <- morie_lggvls
laggedvaliptw  <- morie_lggvls

# cheatsheet
#' Cheatsheet
#'
#' A step of the lggvls_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
#' @examples
#' res <- .lggvls_cheatsheet()
#' res
.lggvls_cheatsheet <- function() {
  paste("lggvls: sustained-exposure IPTW (Robins 1986). Weight =",
        "prod_k f(A_k|Abar_{k-1}) / f(A_k|Abar_{k-1}, Lbar_k,",
        "L_{k-1..k-lag}, Y_{k-1..k-lag}); MSM on cumulative, final",
        "or ever-exposed contrast.")
}
