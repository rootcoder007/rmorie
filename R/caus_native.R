# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Instrumental variables and modern causal estimators.
#
# R mirror of morie.fn.{causiv2sls,causivlim,causivla,causinst,
# causaipw,causdml2,causdidsap}.
#
# Every estimator here is an identification result before it is a
# formula, and the formulas are short enough that the temptation is
# to write them down and stop. The conditions are what the tests
# check: a Wald ratio with a weak first stage is a number divided by
# noise; a k-class estimator with the wrong k is a different
# estimator; a cross-fitted score that was not cross-fitted carries
# the bias it exists to remove; and a two-way fixed-effects event
# study under heterogeneity can return the wrong sign.

.caus_intercept <- function(X) {
  A <- as.matrix(X)
  storage.mode(A) <- "double"
  if (ncol(A) &&
      any(apply(A, 2L, function(cc) isTRUE(all.equal(cc, rep(1, nrow(A))))))) {
    return(A)
  }
  cbind(1, A)
}

# P_Z M by least squares rather than a formed inverse: Z'Z is
# routinely near-singular when instruments are correlated, and
# inverting it explicitly turns a warning into a wrong answer.
.caus_project <- function(Z, M) {
  Z <- as.matrix(Z)
  M <- as.matrix(M)
  Z %*% qr.coef(qr(Z), M)
}

.caus_annihilate <- function(Z, M) as.matrix(M) - .caus_project(Z, M)

# The k-class family. k = 0 is least squares, k = 1 is two-stage
# least squares, and k = the Anderson-Rubin ratio is LIML. Keeping
# them one function is what stops them drifting apart.
.caus_k_class <- function(y, X, Z, k) {
  y <- as.numeric(y)
  X <- as.matrix(X)
  MzX <- .caus_annihilate(Z, X)
  Mzy <- as.numeric(.caus_annihilate(Z, matrix(y, ncol = 1L)))
  A <- crossprod(X) - k * (t(X) %*% MzX)
  b <- crossprod(X, y) - k * crossprod(X, Mzy)
  as.numeric(qr.coef(qr(A), b))
}

.caus_first_stage_f <- function(D, Z) {
  D <- as.numeric(D)
  Zf <- .caus_intercept(Z)
  W <- matrix(1, length(D), 1L)
  rss_r <- sum(.caus_annihilate(W, matrix(D, ncol = 1L))^2)
  rss_u <- sum(.caus_annihilate(Zf, matrix(D, ncol = 1L))^2)
  q <- ncol(Zf) - 1L
  dfr <- length(D) - ncol(Zf)
  if (q < 1L || dfr < 1L || rss_u <= 0) return(NA_real_)
  ((rss_r - rss_u) / q) / (rss_u / dfr)
}


#' Two-stage least squares
#'
#' `beta = (X'P_Z X)^-1 X'P_Z y`. Identification needs at least as
#' many instruments as regressors (the order condition, arithmetic
#' and checked here) and `rank(Z'X) = dim(beta)` (the rank condition,
#' which for floating-point data is a matter of degree rather than a
#' decidable property -- the degree is what `first_stage_F` reports).
#'
#' Residuals are `y - X beta` with the ORIGINAL `X`, not the
#' first-stage fitted values. Using the fitted values there is a
#' common and silent error that produces a smaller number which is
#' not a standard error of anything.
#'
#' @param y outcome.
#' @param X regressors, endogenous and exogenous together.
#' @param Z instruments; exogenous regressors instrument for
#'   themselves and belong in both.
#' @param cluster optional cluster identifiers.
#' @return list: beta, se, t, residuals, fitted, first_stage_F,
#'   overidentified, n_overid_restrictions, sargan, sargan_p,
#'   vcov_type, n, k, m, method.
#' @references Wooldridge (2010), *Econometric Analysis of Cross
#'   Section and Panel Data*, 2nd ed., Ch. 5; Sargan (1958),
#'   *Econometrica* 26:393-415.
#' @examples
#' Z <- matrix(stats::rnorm(200), 100)
#' D <- Z %*% c(1, 0.5) + stats::rnorm(100)
#' morie_caus_iv_2sls(2 * D + stats::rnorm(100), D, Z)$beta
#' @export
morie_caus_iv_2sls <- function(y, X, Z, cluster = NULL) {
  yv <- as.numeric(y)
  Xm <- .caus_intercept(as.matrix(X))
  Zm <- .caus_intercept(as.matrix(Z))
  n <- length(yv)
  k <- ncol(Xm)
  m <- ncol(Zm)
  if (nrow(Xm) != n || nrow(Zm) != n) {
    stop("y, X and Z must agree on the number of rows.", call. = FALSE)
  }
  if (m < k) {
    stop(sprintf(paste("the order condition fails: %d instruments (including",
                       "the constant) for %d regressors. 2SLS is not",
                       "identified."), m, k), call. = FALSE)
  }
  Xhat <- .caus_project(Zm, Xm)
  XtX <- crossprod(Xhat)
  if (qr(XtX)$rank < k) {
    stop(paste("the rank condition fails: the projected regressors are",
               "collinear, so Z carries no independent variation for at",
               "least one endogenous regressor."), call. = FALSE)
  }
  beta <- as.numeric(qr.coef(qr(XtX), crossprod(Xhat, yv)))
  u <- yv - as.numeric(Xm %*% beta)
  bread <- solve(XtX)
  if (is.null(cluster)) {
    meat <- crossprod(Xhat, Xhat * u^2)
    vt <- "heteroskedasticity-robust (HC0)"
  } else {
    cl <- as.vector(cluster)
    if (length(cl) != n) {
      stop(sprintf("cluster has %d entries for %d rows.", length(cl), n),
           call. = FALSE)
    }
    meat <- matrix(0, k, k)
    for (g in unique(cl)) {
      s <- cl == g
      v <- crossprod(Xhat[s, , drop = FALSE], u[s])
      meat <- meat + tcrossprod(v)
    }
    vt <- sprintf("cluster-robust (%d clusters)", length(unique(cl)))
  }
  V <- bread %*% meat %*% bread
  se <- sqrt(pmax(diag(V), 0))
  nres <- m - k
  sargan <- sargan_p <- NULL
  if (nres > 0L) {
    r2 <- 1 - sum(.caus_annihilate(Zm, matrix(u, ncol = 1L))^2) / sum(u^2)
    sargan <- n * r2
    sargan_p <- stats::pchisq(sargan, nres, lower.tail = FALSE)
  }
  excl <- Zm[, !apply(Zm, 2L, function(cc)
    isTRUE(all.equal(cc, rep(1, n)))), drop = FALSE]
  list(beta = beta, se = se, t = ifelse(se > 0, beta / se, NA_real_),
       residuals = u, fitted = as.numeric(Xm %*% beta),
       first_stage_F = .caus_first_stage_f(Xm[, k], excl),
       order_condition = TRUE,
       overidentified = nres > 0L, n_overid_restrictions = nres,
       sargan = sargan, sargan_p = sargan_p, vcov_type = vt,
       residual_note = paste("residuals are y - X beta with the ORIGINAL X;",
                             "using the first-stage fitted values gives a",
                             "smaller number that is not a standard error"),
       n = n, k = k, m = m,
       method = "Two-stage least squares, beta = (X'P_Z X)^-1 X'P_Z y")
}


#' Limited-information maximum likelihood
#'
#' The k-class estimator whose `k` is the Anderson-Rubin variance
#' ratio: `kappa` is the smallest eigenvalue of
#' `(Ybar' M_\[W,Z\] Ybar)^-1 (Ybar' M_W Ybar)`, with `Ybar` holding
#' `y` and the ENDOGENOUS regressors only.
#'
#' Two facts make this checkable. `kappa >= 1` always, and when the
#' model is EXACTLY identified `kappa = 1` and LIML coincides with
#' 2SLS to machine precision. An implementation that does not
#' reproduce 2SLS in the just-identified case is wrong, and nothing
#' else about it needs checking first.
#'
#' `Ybar` must exclude the exogenous columns. An intercept there is
#' annihilated exactly by `M_W`, which makes the matrix singular,
#' drives `kappa` to zero, and silently turns the k-class estimator
#' back into least squares -- a finite, plausible, entirely wrong
#' number.
#'
#' Where LIML and 2SLS differ is with many or weak instruments: 2SLS
#' is biased toward least squares and the bias grows with the
#' instrument count, while LIML is approximately median-unbiased
#' there at the cost of heavier tails.
#'
#' @param y outcome.
#' @param X regressors.
#' @param Z full instrument set.
#' @param fuller optional Fuller `a`, subtracting `a/(n - m)` from
#'   `kappa` to restore finite moments.
#' @param endog optional column indices of `X` that are endogenous;
#'   detected by matching against `Z` when `NULL`.
#' @return list: beta, se, kappa, kappa_minus_one, just_identified,
#'   equals_2sls, endogenous_columns, fuller_a,
#'   n_overid_restrictions, n, k, m, method.
#' @references Anderson and Rubin (1949), *Annals of Mathematical
#'   Statistics* 20:46-63; Fuller (1977), *Econometrica* 45:939-953.
#' @examples
#' Z <- matrix(stats::rnorm(100), 100)
#' D <- Z %*% 1.2 + stats::rnorm(100)
#' morie_caus_iv_liml(2 * D + stats::rnorm(100), D, Z)$kappa
#' @export
morie_caus_iv_liml <- function(y, X, Z, fuller = NULL, endog = NULL) {
  yv <- as.numeric(y)
  Xm <- .caus_intercept(as.matrix(X))
  Zm <- .caus_intercept(as.matrix(Z))
  n <- length(yv)
  k <- ncol(Xm)
  m <- ncol(Zm)
  if (m < k) {
    stop(sprintf("the order condition fails: %d instruments for %d regressors.",
                 m, k), call. = FALSE)
  }
  if (is.null(endog)) {
    is_exog <- logical(k)
    for (j in seq_len(k)) {
      nj <- sqrt(sum(Xm[, j]^2))
      if (nj == 0) {
        is_exog[j] <- TRUE
        next
      }
      for (c in seq_len(m)) {
        if (sqrt(sum((Xm[, j] - Zm[, c])^2)) <= 1e-10 * max(nj, 1)) {
          is_exog[j] <- TRUE
          break
        }
      }
    }
  } else {
    idx <- as.integer(endog)
    if (any(idx < 1L | idx > k)) {
      stop(sprintf("endog indices must lie in 1..%d.", k), call. = FALSE)
    }
    is_exog <- rep(TRUE, k)
    is_exog[idx] <- FALSE
  }
  if (all(is_exog)) {
    stop(paste("no endogenous regressor was identified: every column of X",
               "also appears in Z, so there is nothing to instrument.",
               "Pass endog explicitly if the detection is wrong."),
         call. = FALSE)
  }
  W <- .caus_intercept(Xm[, is_exog, drop = FALSE])
  Ybar <- cbind(yv, Xm[, !is_exog, drop = FALSE])
  A <- crossprod(Ybar, .caus_annihilate(Zm, Ybar))
  B <- crossprod(Ybar, .caus_annihilate(W, Ybar))
  ev <- Re(eigen(solve(A, B), only.values = TRUE)$values)
  ev <- ev[is.finite(ev)]
  if (!length(ev)) {
    stop("the Anderson-Rubin ratio has no finite eigenvalue.", call. = FALSE)
  }
  kappa <- min(ev)
  nres <- m - k
  if (!is.null(fuller)) {
    a <- as.numeric(fuller)
    if (a < 0) {
      stop(sprintf("Fuller's a must be non-negative, got %g.", a),
           call. = FALSE)
    }
    kappa <- kappa - a / (n - m)
  }
  beta <- .caus_k_class(yv, Xm, Zm, kappa)
  u <- yv - as.numeric(Xm %*% beta)
  MzX <- .caus_annihilate(Zm, Xm)
  A2 <- crossprod(Xm) - kappa * (t(Xm) %*% MzX)
  bread <- solve(A2)
  Xt <- Xm - kappa * MzX
  V <- bread %*% crossprod(Xt, Xt * u^2) %*% bread
  list(beta = beta, se = sqrt(pmax(diag(V), 0)), residuals = u,
       kappa = kappa, kappa_minus_one = kappa - 1,
       just_identified = nres == 0L,
       equals_2sls = abs(kappa - 1) < 1e-9,
       endogenous_columns = which(!is_exog),
       fuller_a = if (is.null(fuller)) NULL else as.numeric(fuller),
       n_overid_restrictions = nres,
       kappa_fact = paste("kappa >= 1 always, and equals 1 exactly when the",
                          "model is just identified, where LIML IS 2SLS"),
       n = n, k = k, m = m,
       method = "LIML as the k-class estimator with k = the Anderson-Rubin ratio")
}


#' Local average treatment effect
#'
#' `LATE = (E\[Y|Z=1\] - E\[Y|Z=0\]) / (E\[D|Z=1\] - E\[D|Z=0\])`.
#'
#' Arithmetically the Wald ratio. What Imbens and Angrist established
#' is what it ESTIMATES, and that is the content: under instrument
#' independence, exclusion, a non-zero first stage and MONOTONICITY
#' -- no defiers -- the ratio is the average effect among COMPLIERS
#' alone. That is a different estimand from the average treatment
#' effect, and the compliers are defined by their response to this
#' particular instrument, so a different instrument gives a different
#' subpopulation and a different number.
#'
#' `complier_share` is the denominator and says what fraction of the
#' sample the estimate describes. A LATE from a 4% complier share is
#' a statement about 4% of the sample, however tight its interval.
#'
#' @param y outcome.
#' @param D binary treatment taken.
#' @param Z binary instrument.
#' @return list: late, se, first_stage, reduced_form, complier_share,
#'   n_z1, n_z0, weak_first_stage, estimand, n, method.
#' @references Imbens and Angrist (1994), *Econometrica* 62:467-475;
#'   Angrist, Imbens and Rubin (1996), *JASA* 91:444-455.
#' @examples
#' Z <- stats::rbinom(500, 1, 0.5)
#' D <- Z * stats::rbinom(500, 1, 0.6)
#' morie_caus_iv_late(3 * D + stats::rnorm(500), D, Z)$late
#' @export
morie_caus_iv_late <- function(y, D, Z) {
  yv <- as.numeric(y)
  Dv <- as.numeric(D)
  Zv <- as.numeric(Z)
  if (length(yv) != length(Dv) || length(yv) != length(Zv)) {
    stop("y, D and Z must have the same length.", call. = FALSE)
  }
  for (nm in c("D", "Z")) {
    v <- if (nm == "D") Dv else Zv
    if (!all(v %in% c(0, 1))) {
      stop(sprintf("%s must be binary 0/1 for the LATE theorem.", nm),
           call. = FALSE)
    }
  }
  z1 <- Zv == 1
  z0 <- Zv == 0
  n1 <- sum(z1)
  n0 <- sum(z0)
  if (n1 < 2L || n0 < 2L) {
    stop(sprintf("need at least 2 observations in each arm, got %d and %d.",
                 n1, n0), call. = FALSE)
  }
  rf <- mean(yv[z1]) - mean(yv[z0])
  fs <- mean(Dv[z1]) - mean(Dv[z0])
  if (abs(fs) < 1e-12) {
    stop(paste("the first stage is zero: the instrument does not move",
               "treatment, so the Wald ratio is 0/0 and nothing is",
               "identified."), call. = FALSE)
  }
  late <- rf / fs
  vy <- stats::var(yv[z1]) / n1 + stats::var(yv[z0]) / n0
  vd <- stats::var(Dv[z1]) / n1 + stats::var(Dv[z0]) / n0
  cv <- stats::cov(yv[z1], Dv[z1]) / n1 + stats::cov(yv[z0], Dv[z0]) / n0
  v <- (vy - 2 * late * cv + late^2 * vd) / fs^2
  list(late = late, se = sqrt(max(v, 0)), first_stage = fs,
       reduced_form = rf, complier_share = fs, n_z1 = n1, n_z0 = n0,
       weak_first_stage = abs(fs) < 0.05,
       estimand = paste("the average effect among COMPLIERS, not the",
                        "population average treatment effect"),
       monotonicity_assumed = paste("no defiers: nobody the instrument pushes",
                                    "OUT of treatment. Not testable, but its",
                                    "consequence -- a first stage of one sign",
                                    "throughout -- is visible above"),
       n = length(yv),
       method = "Imbens-Angrist LATE, the Wald ratio under monotonicity")
}


#' The instrumental-variable estimand under a causal graph
#'
#' Arithmetically identical to [morie_caus_iv_late()] -- the two
#' agree to machine precision, and a discrepancy would mean one is
#' wrong. What differs is the assumption set and therefore what the
#' number is ABOUT: `Z -> D -> Y` with no `Z -> Y` arrow (exclusion),
#' no common cause of `Z` and `Y` (exchangeability), and `Z` moving
#' `D` (relevance). With a CONSTANT treatment effect that graph
#' identifies the average treatment effect; without it, the
#' compliers' effect.
#'
#' `homogeneous` records which claim is being made, since the number
#' is the same either way. It defaults to `FALSE` so the weaker,
#' safer reading is what comes out unless constancy is asserted
#' deliberately. Only relevance is testable, and it is reported.
#'
#' @param y outcome.
#' @param D binary treatment.
#' @param Z binary instrument.
#' @param homogeneous assert a constant treatment effect.
#' @return list: beta, se, estimand, homogeneous_asserted, relevance,
#'   relevance_t, relevance_p, assumptions, testable, untestable, n,
#'   method.
#' @references Imbens and Angrist (1994), *Econometrica* 62:467-475;
#'   Angrist, Imbens and Rubin (1996), *JASA* 91:444-455.
#' @examples
#' Z <- stats::rbinom(400, 1, 0.5)
#' D <- Z * stats::rbinom(400, 1, 0.7)
#' morie_caus_iv_dag(2 * D + stats::rnorm(400), D, Z)$relevance
#' @export
morie_caus_iv_dag <- function(y, D, Z, homogeneous = FALSE) {
  o <- morie_caus_iv_late(y, D, Z)
  Dv <- as.numeric(D)
  Zv <- as.numeric(Z)
  z1 <- Zv == 1
  z0 <- Zv == 0
  sd_fs <- sqrt(stats::var(Dv[z1]) / sum(z1) + stats::var(Dv[z0]) / sum(z0))
  tstat <- if (sd_fs > 0) o$first_stage / sd_fs else Inf
  list(beta = o$late, se = o$se,
       estimand = if (isTRUE(homogeneous)) {
         "the average treatment effect, under the asserted constant effect"
       } else {
         paste("the compliers' average effect; NOT the population ATE unless",
               "effects are constant")
       },
       homogeneous_asserted = isTRUE(homogeneous),
       relevance = o$first_stage, relevance_t = tstat,
       relevance_p = 2 * stats::pnorm(abs(tstat), lower.tail = FALSE),
       assumptions = list(
         relevance = "Z moves D",
         exclusion = "no arrow Z -> Y except through D",
         exchangeability = "no common cause of Z and Y",
         homogeneity_or_monotonicity = paste(
           "constant effects gives the ATE; otherwise monotonicity gives the",
           "compliers' effect")),
       testable = "relevance",
       untestable = c("exclusion", "exchangeability", "homogeneity",
                      "monotonicity"),
       same_number_as_late = paste("identical arithmetic to",
                                   "morie_caus_iv_late; only the assumption",
                                   "set and hence the estimand differ"),
       n = o$n,
       method = "Wald / IV estimator under a Z -> D -> Y graph")
}


#' Augmented inverse-probability-weighted (doubly robust) ATE
#'
#' `tau = mean(m1 - m0 + T(y - m1)/e - (1-T)(y - m0)/(1-e))`.
#'
#' The DOUBLE ROBUSTNESS is the property worth having: consistent if
#' EITHER the propensity score or the outcome regressions are
#' correctly specified, not necessarily both. Get both wrong and it
#' is wrong, and no amount of doubly robust machinery rescues that.
#'
#' If `m` is right the augmentation terms have mean zero and this is
#' the regression estimator; if `e` is right the terms rearrange into
#' inverse-probability weighting with the regression as a control.
#'
#' Propensities near 0 or 1 are the practical failure mode, since the
#' estimator divides by them. `trim` bounds them and `n_trimmed`
#' reports how many were touched, because trimming changes the
#' estimand to an average over the retained region.
#'
#' @param y observed outcome.
#' @param T binary treatment.
#' @param ps estimated propensity score.
#' @param m1,m0 estimated outcome regressions for every unit.
#' @param trim propensities are clipped to `\[trim, 1 - trim\]`.
#' @return list: ate, se, ci, influence, regression_component,
#'   augmentation_component, n_trimmed, min_ps, max_ps,
#'   effective_overlap, n, method.
#' @references Robins, Rotnitzky and Zhao (1994), *JASA* 89:846-866;
#'   Bang and Robins (2005), *Biometrics* 61:962-973.
#' @examples
#' n <- 200
#' e <- rep(0.5, n)
#' T <- stats::rbinom(n, 1, e)
#' m0 <- stats::rnorm(n)
#' morie_caus_aipw(m0 + 2 * T, T, e, m0 + 2, m0)$ate
#' @export
morie_caus_aipw <- function(y, T, ps, m1, m0, trim = 0.01) {
  yv <- as.numeric(y)
  Tv <- as.numeric(T)
  e <- as.numeric(ps)
  M1 <- as.numeric(m1)
  M0 <- as.numeric(m0)
  n <- length(yv)
  if (length(Tv) != n || length(e) != n || length(M1) != n ||
        length(M0) != n) {
    stop("y, T, ps, m1 and m0 must have the same length.", call. = FALSE)
  }
  if (!all(Tv %in% c(0, 1))) stop("T must be binary 0/1.", call. = FALSE)
  if (any(e < 0) || any(e > 1)) {
    stop("propensity scores must lie in [0, 1].", call. = FALSE)
  }
  tr <- as.numeric(trim)
  if (tr < 0 || tr >= 0.5) {
    stop(sprintf("trim must lie in [0, 0.5), got %g.", tr), call. = FALSE)
  }
  n_trim <- sum(e < tr | e > 1 - tr)
  ec <- if (tr > 0) pmin(pmax(e, tr), 1 - tr) else e
  if (any(ec <= 0) || any(ec >= 1)) {
    stop(paste("a propensity score is exactly 0 or 1, so a unit has no",
               "counterfactual and the estimator divides by zero; use",
               "trim > 0 or drop the unit."), call. = FALSE)
  }
  reg <- M1 - M0
  aug <- Tv * (yv - M1) / ec - (1 - Tv) * (yv - M0) / (1 - ec)
  infl <- reg + aug
  ate <- mean(infl)
  se <- stats::sd(infl) / sqrt(n)
  list(ate = ate, se = se,
       ci = c(ate - stats::qnorm(0.975) * se, ate + stats::qnorm(0.975) * se),
       influence = infl, regression_component = mean(reg),
       augmentation_component = mean(aug),
       n_trimmed = n_trim, min_ps = min(e), max_ps = max(e),
       effective_overlap = mean(e > 0.1 & e < 0.9),
       doubly_robust = paste("consistent if EITHER the propensity score or",
                             "the outcome regressions are correct, not",
                             "necessarily both; wrong on both and it is wrong"),
       trimming_note = paste("trimming changes the estimand to an average",
                             "over the retained region, not the whole",
                             "population"),
       n = n,
       method = "AIPW / doubly robust ATE (Robins, Rotnitzky and Zhao 1994)")
}


#' Double machine learning for the partially linear model
#'
#' `Y = theta D + g(X) + eps`, `D = m(X) + nu`, estimated by
#' `theta = (Dtilde'Dtilde)^-1 Dtilde'Ytilde` with both residuals
#' CROSS-FITTED: each observation's nuisance prediction comes from a
#' model fitted without it.
#'
#' Two ingredients, neither optional. Residualising BOTH `Y` and `D`
#' on `X` makes the score Neyman-orthogonal, so first-order errors in
#' the nuisance estimates do not contaminate `theta`; residualising
#' only `Y` and regressing on raw `D` is not orthogonal and the bias
#' returns. And cross-fitting removes a regularisation bias that
#' otherwise does not vanish at root-n. `theta_in_sample` is computed
#' alongside precisely so the gap is visible.
#'
#' @param y outcome.
#' @param D treatment.
#' @param X controls.
#' @param n_folds cross-fitting folds, at least 2.
#' @param learner optional `function(Xtr, ytr)` returning a predict
#'   function; a ridge fit when `NULL`.
#' @param seed fold-assignment seed.
#' @return list: theta, se, ci, y_residual, d_residual,
#'   theta_in_sample, cross_fitted, n_folds, first_stage_r2, n, p,
#'   method.
#' @references Chernozhukov, Chetverikov, Demirer, Duflo, Hansen,
#'   Newey and Robins (2018), *Econometrics Journal* 21:C1-C68.
#' @examples
#' X <- matrix(stats::rnorm(600), 200)
#' D <- X[, 1] + stats::rnorm(200)
#' morie_caus_dml_partial_lin(1.5 * D + X[, 1], D, X, n_folds = 3)$theta
#' @export
morie_caus_dml_partial_lin <- function(y, D, X, n_folds = 5, learner = NULL,
                                       seed = 0) {
  yv <- as.numeric(y)
  Dv <- as.numeric(D)
  Xm <- as.matrix(X)
  storage.mode(Xm) <- "double"
  if (nrow(Xm) != length(yv)) Xm <- t(Xm)
  n <- length(yv)
  p <- ncol(Xm)
  if (length(Dv) != n || nrow(Xm) != n) {
    stop("y, D and X must agree on the number of rows.", call. = FALSE)
  }
  nf <- as.integer(n_folds)
  if (is.na(nf) || nf < 2L || nf > n) {
    stop(sprintf(paste("n_folds must lie in 2..%d, got %s; cross-fitting",
                       "with one fold is not cross-fitting."), n,
                 format(n_folds)), call. = FALSE)
  }
  fit <- if (is.null(learner)) function(Xtr, ytr) {
    Xc <- cbind(1, Xtr)
    pen <- diag(ncol(Xc))
    pen[1L, 1L] <- 0
    b <- solve(crossprod(Xc) + pen, crossprod(Xc, ytr))
    function(Xn) as.numeric(cbind(1, Xn) %*% b)
  } else learner

  old <- if (exists(".Random.seed", envir = globalenv())) {
    get(".Random.seed", envir = globalenv())
  } else NULL
  set.seed(seed)
  on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = globalenv()))
  perm <- sample.int(n)
  fs <- lapply(seq_len(nf), function(i) perm[seq(i, n, by = nf)])

  yres <- numeric(n)
  dres <- numeric(n)
  for (te in fs) {
    tr <- setdiff(seq_len(n), te)
    yres[te] <- yv[te] - fit(Xm[tr, , drop = FALSE], yv[tr])(
      Xm[te, , drop = FALSE])
    dres[te] <- Dv[te] - fit(Xm[tr, , drop = FALSE], Dv[tr])(
      Xm[te, , drop = FALSE])
  }
  den <- sum(dres^2)
  if (den <= 0) {
    stop(paste("the residualised treatment has no variation left: X explains",
               "D completely, so theta is not identified."), call. = FALSE)
  }
  theta <- sum(dres * yres) / den
  eps <- yres - theta * dres
  se <- sqrt(sum(dres^2 * eps^2)) / den
  yr_in <- yv - fit(Xm, yv)(Xm)
  dr_in <- Dv - fit(Xm, Dv)(Xm)
  din <- sum(dr_in^2)
  list(theta = theta, se = se,
       ci = c(theta - stats::qnorm(0.975) * se,
              theta + stats::qnorm(0.975) * se),
       y_residual = yres, d_residual = dres,
       theta_in_sample = if (din > 0) sum(dr_in * yr_in) / din else NA_real_,
       cross_fitted = TRUE, n_folds = nf,
       first_stage_r2 = if (stats::var(Dv) > 0) {
         1 - stats::var(dres) / stats::var(Dv)
       } else NA_real_,
       why_cross_fit = paste("fitting the nuisances on the data used for the",
                             "final moment leaves a regularisation bias that",
                             "does not vanish at root-n"),
       why_orthogonal = paste("residualising BOTH Y and D on X makes the",
                              "score Neyman-orthogonal; residualising only Y",
                              "is not orthogonal and the bias returns"),
       n = n, p = p,
       method = "Double machine learning, partially linear model")
}


#' Sun-Abraham interaction-weighted event study
#'
#' The usual two-way fixed-effects event study reads its relative-time
#' coefficients as dynamic treatment effects. Sun and Abraham show
#' each is a weighted sum of cohort-specific effects at MANY relative
#' times with possibly NEGATIVE weights, so under heterogeneity
#' across cohorts it can carry the wrong sign -- the contamination
#' comes from already-treated units acting as controls.
#'
#' The interaction-weighted estimator never pools across cohorts. It
#' forms a cohort-by-relative-time effect against a clean control
#' group and aggregates with the SHARES of each cohort among units
#' observed at that relative time. Because the weights are shares
#' they are non-negative and sum to one, which is exactly what the
#' two-way fixed-effects weights are not. `naive_twfe` is computed
#' alongside for comparison.
#'
#' Period `e - 1` is the reference, so `mu` at relative time -1 is
#' zero by construction and the pre-periods are a pre-trends check.
#'
#' @param Y_panel balanced panel, units by periods.
#' @param G_first_treat first-treatment period per unit; `Inf` or
#'   `NA` for never-treated.
#' @param rel_periods relative periods to report.
#' @param control `"never"` or `"notyet"`.
#' @return list: rel_periods, mu, catt, weights, cohorts, naive_twfe,
#'   weights_nonnegative, weights_sum_to_one, n_never_treated,
#'   control_group, n_units, n_periods, method.
#' @references Sun and Abraham (2021), *Journal of Econometrics*
#'   225:175-199.
#' @examples
#' Y <- matrix(stats::rnorm(200), 20)
#' G <- c(rep(4, 10), rep(Inf, 10))
#' morie_caus_did_sun_abraham(Y, G, rel_periods = c(0, 1))$weights_sum_to_one
#' @export
morie_caus_did_sun_abraham <- function(Y_panel, G_first_treat,
                                       rel_periods = NULL,
                                       control = "never") {
  Y <- as.matrix(Y_panel)
  storage.mode(Y) <- "double"
  G <- as.numeric(G_first_treat)
  n <- nrow(Y)
  Tn <- ncol(Y)
  if (length(G) != n) {
    stop(sprintf("G_first_treat has %d entries for %d units.", length(G), n),
         call. = FALSE)
  }
  if (!control %in% c("never", "notyet")) {
    stop("control must be 'never' or 'notyet'.", call. = FALSE)
  }
  never <- !is.finite(G)
  if (identical(control, "never") && !any(never)) {
    stop(paste("no never-treated units, so there is no clean control group;",
               "use control='notyet'."), call. = FALSE)
  }
  cohorts <- sort(unique(G[is.finite(G)]))
  cohorts <- cohorts[cohorts >= 2 & cohorts <= Tn]
  if (!length(cohorts)) {
    stop(paste("no cohort is treated at a period with both a pre-period and",
               "a post-period, so no effect is estimable."), call. = FALSE)
  }
  rel <- if (is.null(rel_periods)) {
    seq(max(-5, -(min(cohorts) - 1)), min(5, Tn - min(cohorts)))
  } else as.integer(rel_periods)

  catt <- matrix(NA_real_, length(cohorts), length(rel))
  wts <- matrix(0, length(cohorts), length(rel))
  for (ci in seq_along(cohorts)) {
    e <- cohorts[ci]
    treated <- G == e
    for (li in seq_along(rel)) {
      t <- e + rel[li]
      if (t < 1 || t > Tn || e - 1 < 1) next
      ctrl <- if (identical(control, "never")) never else {
        ((G > t) | never) & !treated
      }
      if (!any(ctrl) || !any(treated)) next
      catt[ci, li] <-
        (mean(Y[treated, t]) - mean(Y[treated, e - 1])) -
        (mean(Y[ctrl, t]) - mean(Y[ctrl, e - 1]))
      wts[ci, li] <- sum(treated)
    }
  }
  wts[is.na(catt)] <- 0
  col <- colSums(wts)
  for (li in seq_along(rel)) {
    if (col[li] > 0) wts[, li] <- wts[, li] / col[li]
  }
  mu <- vapply(seq_along(rel), function(li) {
    if (col[li] <= 0) return(NA_real_)
    sum(wts[, li] * ifelse(is.na(catt[, li]), 0, catt[, li]))
  }, numeric(1))

  ever <- is.finite(G)
  Yd <- Y - rowMeans(Y) - rep(colMeans(Y), each = n) + mean(Y)
  naive <- vapply(rel, function(l) {
    cells <- numeric(0)
    for (i in seq_len(n)) {
      if (!ever[i]) next
      t <- G[i] + l
      if (t >= 1 && t <= Tn) cells <- c(cells, Yd[i, t])
    }
    if (length(cells)) mean(cells) else NA_real_
  }, numeric(1))

  list(rel_periods = rel, mu = mu, catt = catt, weights = wts,
       cohorts = cohorts, naive_twfe = naive,
       weights_nonnegative = all(wts >= -1e-12),
       weights_sum_to_one = all(abs(colSums(wts)[col > 0] - 1) < 1e-9),
       reference_period = -1L,
       n_never_treated = sum(never), control_group = control,
       why_not_twfe = paste("a two-way fixed-effects event-study coefficient",
                            "is a weighted sum of cohort effects at MANY",
                            "relative times with possibly NEGATIVE weights,",
                            "so under heterogeneity it can carry the wrong",
                            "sign; the interaction weights are shares and",
                            "cannot"),
       n_units = n, n_periods = Tn,
       method = "Sun-Abraham interaction-weighted event study (2021)")
}
