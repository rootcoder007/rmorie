# morie.fn -- function file (rootcoder007/morie)
# R translation: MSM for a continuous-dose treatment.
# References:
#   Robins, J. M., Hernan, M. A. & Brumback, B. (2000) "Marginal structural
#     models and causal inference in epidemiology", Epidemiology 11(5), 550-560.
#   Imai, K. & van Dyk, D. A. (2004) "Causal inference with general treatment
#     regimes: generalizing the propensity score", JASA 99(467), 854-866.
#   Hirano, K. & Imbens, G. W. (2004) "The propensity score with continuous
#     treatments", in Applied Bayesian Modeling and Causal Inference from
#     Incomplete-Data Perspectives, Wiley, 73-84.
#   Hernan M. A. & Robins J. M. (2020) Causal Inference: What If, Sec. 12.3.

.gentmt_vec <- function(x) {
  as.numeric(x)
}

.gentmt_ols_core <- function(X, y) {
  n <- nrow(X)
  X_full <- cbind(1, X)
  p <- ncol(X_full)
  XtX <- crossprod(X_full)
  XtX_inv <- solve(XtX)
  beta <- as.numeric(XtX_inv %*% crossprod(X_full, y))
  fitted <- as.numeric(X_full %*% beta)
  resid <- y - fitted
  sigma2 <- sum(resid^2) / (n - p)
  vcov <- sigma2 * XtX_inv
  se <- sqrt(diag(vcov))
  list(coef = beta, se = se, vcov = vcov, fitted = fitted,
       residuals = resid, sigma2 = sigma2)
}

.gentmt_wls <- function(X, y, w) {
  n <- nrow(X)
  X_full <- cbind(1, X)
  p <- ncol(X_full)
  sw <- sqrt(w)
  Xw <- X_full * sw
  yw <- y * sw
  XtX <- crossprod(Xw)
  XtX_inv <- solve(XtX)
  beta <- as.numeric(XtX_inv %*% crossprod(Xw, yw))
  fitted <- as.numeric(X_full %*% beta)
  resid <- y - fitted
  wrss <- sum(resid^2 * w)
  sigma2 <- wrss / (n - p)
  vcov <- sigma2 * XtX_inv
  se <- sqrt(diag(vcov))
  list(coef = beta, se = se, vcov = vcov, fitted = fitted,
       residuals = resid, sigma2 = sigma2)
}

.gentmt_treatment_density <- function(A, H, kind = "normal") {
  A <- .gentmt_vec(A)
  H <- as.matrix(H)
  fit <- .gentmt_ols_core(H, A)
  mu <- fit$fitted
  sigma2 <- fit$sigma2
  sigma <- sqrt(sigma2)
  dens <- dnorm(A, mean = mu, sd = sigma)
  list(dens = dens, info = list(mu = mu, sigma2 = sigma2))
}

.gentmt_ip_weights <- function(A, H, kind = "normal", stabilize = TRUE, trim = NULL) {
  td <- .gentmt_treatment_density(A, H, kind)
  mu <- td$info$mu
  s2 <- td$info$sigma2
  sigma <- sqrt(s2)
  denom <- dnorm(A, mean = mu, sd = sigma)
  if (isTRUE(stabilize)) {
    marg_mean <- mean(A)
    marg_sd <- sd(A)
    num <- dnorm(A, mean = marg_mean, sd = marg_sd)
    w <- num / denom
  } else {
    w <- 1 / denom
  }
  if (!is.null(trim) && is.numeric(trim) && trim > 0 && trim < 0.5) {
    lo <- as.numeric(quantile(w, trim))
    hi <- as.numeric(quantile(w, 1 - trim))
    w <- pmin(pmax(w, lo), hi)
  }
  mean_w <- mean(w)
  max_w <- max(w)
  ess <- sum(w)^2 / sum(w^2)
  marg_var <- var(A)
  finite_var <- s2 > 0.5 * marg_var
  var_ratio <- s2 / marg_var
  list(w = w, info = list(
    mean_weight = mean_w,
    max_weight = max_w,
    effective_sample_size = ess,
    finite_variance = finite_var,
    variance_ratio = var_ratio,
    denominator = denom
  ))
}

.gentmt_gps_subclassify <- function(y, A, H, n_strata = 5, degree = 1) {
  yv <- .gentmt_vec(y)
  av <- .gentmt_vec(A)
  n <- length(yv)
  J <- as.integer(n_strata)
  deg <- as.integer(degree)
  if (J < 2) {
    stop(sprintf("gps_subclassify: need at least 2 strata, got %s", n_strata))
  }
  if (n < 4 * J) {
    stop(sprintf("gps_subclassify: %d observations cannot support %d strata; each needs enough points to fit a degree-%d dose model",
                 n, J, deg))
  }
  td <- .gentmt_treatment_density(av, H, "normal")
  mu <- td$info$mu
  order_idx <- order(mu)
  edges <- round((0:J) * n / J)
  slopes <- numeric(0)
  sizes <- integer(0)
  ses <- numeric(0)
  for (j in seq_len(J)) {
    start <- edges[j] + 1
    end <- edges[j + 1]
    if (end < start) next
    idx <- order_idx[start:end]
    if (length(idx) < deg + 2) next
    av_idx <- av[idx]
    Xs <- sapply(seq_len(deg), function(d) av_idx^d)
    ysel <- yv[idx]
    f <- .gentmt_wls(Xs, ysel, rep(1, length(idx)))
    slopes <- c(slopes, f$coef[2])
    ses <- c(ses, f$se[2])
    sizes <- c(sizes, length(idx))
  }
  if (length(slopes) == 0) {
    stop("gps_subclassify: every stratum was too small to fit; reduce n_strata")
  }
  tot <- sum(sizes)
  est <- sum(slopes * sizes) / tot
  var_est <- sum((sizes / tot)^2 * ses^2)
  se <- if (var_est > 0) sqrt(var_est) else NaN
  list(
    estimate = est,
    se = se,
    stratum_slopes = slopes,
    stratum_sizes = as.integer(sizes),
    stratum_se = ses,
    gps_mean = mu,
    n_strata = length(slopes),
    n = n,
    degree = deg
  )
}

.gentmt_dose_response_curve <- function(y, A, H, doses = NULL, degree = 1) {
  yv <- .gentmt_vec(y)
  av <- .gentmt_vec(A)
  n <- length(yv)
  deg <- as.integer(degree)
  td <- .gentmt_treatment_density(av, H, "normal")
  dens <- td$dens
  mu <- td$info$mu
  s2 <- td$info$sigma2

  gps_at <- function(a, i) {
    r <- a - mu[i]
    exp(-0.5 * r * r / s2) / sqrt(2 * pi * s2)
  }

  X <- cbind(
    sapply(seq_len(deg), function(d) av^d),
    dens, dens^2, av * dens
  )
  fit <- .gentmt_wls(X, yv, rep(1, n))
  b <- fit$coef

  if (is.null(doses)) {
    lo <- min(av)
    hi <- max(av)
    doses <- seq(lo, hi, length.out = 21)
  }
  doses <- as.numeric(doses)

  curve <- numeric(length(doses))
  for (k_idx in seq_along(doses)) {
    a <- doses[k_idx]
    tot <- 0
    for (i in seq_len(n)) {
      r <- gps_at(a, i)
      row <- c(1, a^seq_len(deg), r, r^2, a * r)
      tot <- tot + sum(b * row)
    }
    curve[k_idx] <- tot / n
  }

  slopes <- numeric(0)
  nd <- length(doses)
  if (nd > 1) {
    for (t in seq_len(nd - 1)) {
      if (doses[t + 1] != doses[t]) {
        slopes <- c(slopes, (curve[t + 1] - curve[t]) / (doses[t + 1] - doses[t]))
      }
    }
  }

  est <- if (length(slopes) > 0) mean(slopes) else NaN

  list(
    estimate = est,
    se = NaN,
    doses = doses,
    curve = curve,
    slopes = slopes,
    coef = b,
    gps = dens,
    n = n,
    degree = deg
  )
}

#' morie_gentmt
#'
#' Part of the gentmt_native implementation; see the file header for the
#' source it follows.
#'
#' @param y See Usage.
#' @param A See Usage.
#' @param H See Usage.
#' @param method Defaults to \code{"weight"}.
#' @param degree Defaults to \code{1}.
#' @param n_strata Defaults to \code{5}.
#' @param doses Defaults to \code{NULL}.
#' @param trim Defaults to \code{NULL}.
#' @return The value of \code{out}, as built in the body.
#' @export
morie_gentmt <- function(y, A, H, method = "weight", degree = 1,
                         n_strata = 5, doses = NULL, trim = NULL) {
  methods <- c("weight", "subclassify", "doseresponse")
  if (!(method %in% methods)) {
    stop(sprintf("generalized_treatment_msm: method must be one of %s, got '%s'",
                 paste(sprintf("'%s'", methods), collapse = ", "), method))
  }
  deg <- as.integer(degree)
  if (deg < 1) {
    stop(sprintf("generalized_treatment_msm: degree must be at least 1, got %s", degree))
  }
  yv <- .gentmt_vec(y)
  av <- .gentmt_vec(A)
  n <- length(yv)
  if (length(av) != n) {
    stop(sprintf("generalized_treatment_msm: %d outcomes but %d doses", n, length(av)))
  }
  nu <- length(unique(av))
  if (nu < 3) {
    stop(sprintf("generalized_treatment_msm: the dose takes %d distinct values; this is the continuous-treatment estimator and a binary or near-binary exposure belongs in a binary MSM",
                 nu))
  }

  if (method == "weight") {
    ipw <- .gentmt_ip_weights(av, H, kind = "normal", stabilize = TRUE, trim = trim)
    w <- ipw$w
    info <- ipw$info
    X <- sapply(seq_len(deg), function(d) av^d)
    fit <- .gentmt_wls(X, yv, w)
    crude <- .gentmt_wls(X, yv, rep(1, n))
    result <- list(
      estimate = fit$coef[2],
      se = fit$se[2],
      coef = fit$coef,
      vcov = fit$vcov,
      crude = crude$coef[2],
      weights = w,
      mean_weight = info$mean_weight,
      max_weight = info$max_weight,
      effective_sample_size = info$effective_sample_size,
      finite_variance = info$finite_variance,
      variance_ratio = info$variance_ratio,
      gps = info$denominator,
      degree = deg,
      n = n,
      method = "stabilized IP weighting for a continuous dose, Robins, Hernan & Brumback (2000)"
    )
    return(result)
  }

  if (method == "subclassify") {
    out <- .gentmt_gps_subclassify(yv, av, H, n_strata = n_strata, degree = deg)
    out$method <- "generalized propensity score subclassification, Imai & van Dyk (2004)"
    return(out)
  }

  out <- .gentmt_dose_response_curve(yv, av, H, doses = doses, degree = deg)
  out$method <- "dose-response function via the GPS, Hirano & Imbens (2004)"
  return(out)
}

.gentmt_cheatsheet <- function() {
  paste0("gentmt: continuous-dose MSM. weight = SW = f(A)/f(A|L) ",
         "(Robins-Hernan-Brumback 2000, default); subclassify = GPS ",
         "strata (Imai-van Dyk 2004); doseresponse = E[Y^a] curve ",
         "(Hirano-Imbens 2004). Reports finite_variance.")
}

# compact alias per ledger/NAMING.md
morie_gentmt_compact <- morie_gentmt
