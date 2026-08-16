# Marginal structural model with regularized propensity weights.
# Sources: Setoguchi, S., Schneeweiss, S., Brookhart, M. A., Glynn,
# R. J. & Cook, E. F. (2008) "Evaluating uses of data mining
# techniques in propensity score estimation: a simulation study",
# Pharmacoepidemiology and Drug Safety 17(6), 546-555,
# doi:10.1002/pds.1555; Westreich, D., Lessler, J. & Funk, M. J.
# (2010) "Propensity score estimation: neural networks, support
# vector machines, decision trees (CART), and meta-classifiers as
# alternatives to logistic regression", Journal of Clinical
# Epidemiology 63(8), 826-833, doi:10.1016/j.jclinepi.2009.11.020;
# Hernan, M. A. & Robins, J. M. (2020) Causal Inference: What If,
# Sec. 12.3 -- the stabilized weights and the mean-1 diagnostic.
#
# Native R arm mirroring the Python arm exactly: the same ridge
# penalty applied to the propensity slopes (not the intercept), the
# same stabilized weights, the same weighted least squares fit of
# the MSM, the same exposure contrasts (cumulative / final /
# everexposed), and the same diagnostics (effective sample size and
# maximum weight) along a path of penalties that runs from the
# unpenalized fit to the unadjusted limit.

#' .vec
#'
#' A step of the shdsmw_native implementation. Called by \code{.shdsmw_wls}, \code{shrinkage_msm}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x A matrix; passed to \code{as.matrix}.
#' @return A vector, from \code{as.numeric}.
#' @export
.vec <- function(x) as.numeric(as.matrix(x))

#' .hist
#'
#' A step of the shdsmw_native implementation. Called by \code{shrinkage_msm}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param obj Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.
#' @param allow_none A flag; the body branches on it. Defaults to \code{FALSE}.
#' @return The value of \code{list}.
#' @export
.hist <- function(obj, allow_none = FALSE) {
  if (is.null(obj)) return(if (allow_none) list(NULL) else list())
  if (is.list(obj) && (length(obj) == 0L ||
                       is.null(obj[[1L]]) || is.matrix(obj[[1L]]) ||
                       is.numeric(obj[[1L]]))) {
    return(obj)
  }
  list(obj)
}

#' .shdsmw_wls
#'
#' A step of the shdsmw_native implementation. Called by \code{shrinkage_msm}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; passed to \code{nrow}.
#' @param y A matrix; passed to \code{crossprod}.
#' @param w Numeric; combined arithmetically in the body.
#' @return A list with \code{coef}, \code{se}.
#' @export
.shdsmw_wls <- function(X, y, w) {
  X <- as.matrix(X); storage.mode(X) <- "double"
  y <- .vec(y); w <- .vec(w)
  n <- length(y)
  if (nrow(X) != n) stop("X and y length mismatch")
  # diagonal weight matrix; weighted normal equations X' W X b = X' W y
  WX <- sweep(X, 1, w, "*")
  A <- crossprod(WX, X)
  b <- crossprod(WX, y)
  Ainv <- tryCatch(solve(A), error = function(e) {
    stop("singular weighted system -- is the design rank-deficient?")
  })
  coef <- as.numeric(Ainv %*% b)
  fitted <- as.numeric(X %*% coef)
  resid <- y - fitted
  # variance of coefficients: sigma^2 (X' W X)^-1 with sigma^2 estimated
  # from the weighted residual sum of squares / (n - p).
  p <- ncol(X)
  rss <- sum(w * resid^2)
  sigma2 <- if (n > p) rss / (n - p) else NA_real_
  var_b <- if (is.na(sigma2)) rep(NA_real_, p) else
    as.numeric(sigma2) * diag(Ainv)
  list(coef = coef, se = sqrt(pmax(var_b, 0)))
}

#' IP-weighted MSM with a ridge-penalized propensity model
#' @export
shrinkage_msm <- function(y, treatment_history, covariate_history,
                          lam = 0.0, contrast = "cumulative",
                          path = NULL, trim = NULL) {
  if (as.numeric(lam) < 0) {
    stop("shrinkage_msm: lam must be non-negative, got ",
         deparse(lam))
  }
  A_hist <- .hist(treatment_history)
  L_hist <- .hist(covariate_history, allow_none = TRUE)
  if (length(L_hist) != length(A_hist)) {
    stop("shrinkage_msm: ", length(A_hist), " treatment times but ",
         length(L_hist), " covariate blocks")
  }
  yv <- .vec(y)
  n <- length(yv)

  # ridge-penalised logistic regression on the treatment history
  # using the supplied previous covariates, with a non-negative
  # penalty on the slopes (NOT the intercept). Returns fitted
  # probabilities of length n.
  ridge_ps <- function(A_block, L_block, penalty) {
    A <- .vec(A_block)
    # Build the design: intercept + the most recent L (or empty).
    if (is.null(L_block) || length(L_block) == 0L) {
      X <- matrix(1, nrow = n, ncol = 1L)
    } else {
      Lm <- as.matrix(L_block); storage.mode(Lm) <- "double"
      X <- cbind(1, Lm)
    }
    p <- ncol(X)
    if (p == 1L) {
      m <- mean(A)
      p1 <- max(min(m, 1 - 1e-12), 1e-12)
      return(rep(p1, n))
    }
    pen <- rep(0, p); pen[-1L] <- penalty
    eta <- as.numeric(X[, -1L, drop = FALSE] %*%
                      rep(0, p - 1L))
    mu <- rep(mean(A), n)
    for (it in seq_len(50L)) {
      pi_ <- 1 / (1 + exp(-(X %*% c(log(mu / (1 - mu)), rep(0, p - 1L)) +
                            eta)))
      pi_ <- pmin(pmax(pi_, 1e-12), 1 - 1e-12)
      # one Newton step with ridge
      W <- pi_ * (1 - pi_)
      XW <- sweep(X, 1, W, "*")
      H <- crossprod(XW, X) + diag(pen, p)
      g <- crossprod(XW, (A - pi_)) - pen *
        c(0, rep(0, p - 1L))
      step <- tryCatch(solve(H, g), error = function(e) rep(0, p))
      b <- step
      mu <- 1 / (1 + exp(-(X %*% b)))
    }
    mu
  }

  fit_at <- function(lm) {
    w <- numeric(n); per <- list()
    for (t in seq_along(A_hist)) {
      L_block <- if (length(L_hist) >= t) L_hist[[t]] else NULL
      ps <- ridge_ps(A_hist[[t]], L_block, penalty = lm)
      A <- .vec(A_hist[[t]])
      sw <- ifelse(A == 1, 1 / ps, 1 / (1 - ps))
      if (!is.null(trim)) sw <- pmin(sw, trim)
      w <- w + sw
      per[[t]] <- ps
    }
    cum <- numeric(n)
    for (i in seq_len(n)) for (a in A_hist) cum[i] <- cum[i] + .vec(a)[i]
    e <- switch(contrast,
                cumulative = cum,
                final = .vec(A_hist[[length(A_hist)]]),
                everexposed = ifelse(cum > 0, 1, 0),
                stop("shrinkage_msm: contrast must be 'cumulative', ",
                     "'final' or 'everexposed', got ",
                     deparse(contrast)))
    X <- matrix(e, n, 1L)
    f <- .shdsmw_wls(X, yv, w)
    s1 <- sum(w); s2 <- sum(w * w)
    list(lam = as.numeric(lm), estimate = f$coef[1L],
         se = f$se[1L], weights = w,
         mean_weight = s1 / n, max_weight = max(w),
         effective_sample_size =
           if (s2 > 0) (s1 * s1 / s2) else 0,
         exposure = e)
  }

  main <- fit_at(lam)
  if (is.null(path)) path <- c(0, 1e-4, 1e-2, 0.1, 1.0, 10.0, 1e3)
  rows <- list()
  for (lm in path) {
    r <- fit_at(lm)
    rows[[length(rows) + 1L]] <- list(lam = r$lam, estimate = r$estimate,
                                      se = r$se,
                                      max_weight = r$max_weight,
                                      effective_sample_size =
                                        r$effective_sample_size)
  }
  unadj <- .shdsmw_wls(matrix(main$exposure, n, 1L), yv, rep(1, n))
  out <- main
  out$path <- rows
  out$unadjusted <- unadj$coef[1L]
  out$n <- n
  out$n_times <- length(A_hist)
  out$contrast <- contrast
  out$method <- paste("MSM with ridge-penalized propensity weights, ",
                      "Setoguchi et al. (2008) and Westreich, Lessler & ",
                      "Funk (2010); weights per Hernan & Robins (2020) ",
                      "Sec. 12.3")
  out
}

#' Just the penalty path, for sensitivity-only questions
#' @export
penalty_path <- function(y, treatment_history, covariate_history,
                         path = NULL, contrast = "cumulative") {
  r <- shrinkage_msm(y, treatment_history, covariate_history, lam = 0.0,
                    contrast = contrast, path = path)
  r$path
}

# house entry point: the package exports one morie_<module>
morie_shdsmw <- shrinkage_msm
