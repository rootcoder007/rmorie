# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Causal inference shelf -- R mirror of the Python modules gstabwt,
# stbciw, eaiprl, caustmle, cde, snmlin.
#
# Sources consulted, not recalled:
#   Zubizarreta, Stuart, Small & Rosenbaum (eds), Handbook of Matching
#   and Weighting Adjustments for Causal Inference, Chapman & Hall/CRC.
#   Read from the corpus PDF: the IP weight and its stabilized form,
#   ch.18 p.364; the augmented IPW estimator eq. (23.4) p.512.
#   Gruber & van der Laan (2012), tmle: An R Package for Targeted
#   Maximum Likelihood Estimation, J Stat Softw 51(13) pp.5-6 --
#   fetched and read: clever covariates (2)-(3) and the logistic
#   fluctuation of the targeting step.
#   Robins, Hernan & Brumback (2000) Epidemiology 11:550-560;
#   Robins (1993) Proc Biopharm Sect ASA 24-33;
#   Robins, Rotnitzky & Zhao (1994) JASA 89:846-866;
#   Robins & Greenland (1992) Epidemiology 3:143-155;
#   Robins (1994) Commun Stat 23:2379-2412.
#
# Nuisance quantities (propensities, outcome fits, censoring
# probabilities) are supplied by the caller, never fitted inside these
# functions.  That keeps every one of them deterministic and leaves
# the modelling choice where it belongs.  The only iterative step is
# the TMLE fluctuation, which runs a fixed 100 Newton steps with no
# tolerance-driven early exit.
#
# Collision scan: causalw.R and all six exported names were free in
# both R trees and in _lazy_map.json at the time of writing.

#' Stabilized inverse-probability-of-treatment weights
#'
#' \deqn{sw_i = \prod_t f(A_{it}|A_{i,t-1}) / f(A_{it}|H_{it})}{sw = prod_t f(A_t|A_(t-1)) / f(A_t|H_t)}
#' The marginal numerator keeps the weights centred near one instead of
#' letting one small conditional probability blow the weight up.
#' Correctly specified stabilized weights have mean close to 1, so
#' `mean_weight` is the diagnostic to look at.
#'
#' @param treatment,history Accepted for signature compatibility; used
#'   only to check shapes.
#' @param numerator_model Fitted \eqn{f(A_t|A_{t-1})}, subjects by time.
#' @param denominator_model Fitted \eqn{f(A_t|H_t)}, subjects by time.
#' @return Named list with `estimate`, `weights`, `unstabilized`,
#'   `mean_weight`, `max_weight`, `n`, `n_times`, `method`.
#' @references Robins, Hernan & Brumback (2000) Epidemiology 11:550-560.
#' @examples
#' Gstabwt(denominator_model = matrix(c(0.3, 0.6, 0.7, 0.2), nrow = 2))
#' @export
Gstabwt <- function(treatment = NULL, history = NULL,
                    numerator_model = NULL, denominator_model = NULL) {
  if (is.null(denominator_model))
    stop("denominator_model (f(A_t | H_t)) is required", call. = FALSE)
  D <- as.matrix(denominator_model); storage.mode(D) <- "double"
  n <- nrow(D); nt <- ncol(D)
  if (n == 0) stop("need at least one subject", call. = FALSE)
  if (any(D <= 0)) stop("denominator probabilities must be positive",
                        call. = FALSE)
  N <- if (is.null(numerator_model)) matrix(1, n, nt) else {
    M <- as.matrix(numerator_model); storage.mode(M) <- "double"
    if (nrow(M) != n || ncol(M) != nt)
      stop("numerator_model must match denominator_model", call. = FALSE)
    M
  }
  if (!is.null(treatment) && NROW(treatment) != n)
    stop("treatment must have one row per subject", call. = FALSE)
  den <- apply(D, 1, prod); num <- apply(N, 1, prod)
  w <- num / den
  list(estimate = sum(w) / n, weights = w, unstabilized = 1 / den,
       mean_weight = sum(w) / n, max_weight = max(w), n = n, n_times = nt,
       method = "stabilized IPT weights (Robins, Hernan & Brumback 2000)")
}

#' Stabilized inverse-probability-of-censoring weights
#'
#' The same product form as the treatment weight, with the probability
#' of remaining uncensored in place of the treatment probability.
#' Uncensored subjects stand in for those who were censored.
#'
#' @param C Conditional probabilities of remaining uncensored,
#'   subjects by time.
#' @param H Accepted for signature compatibility; shape check only.
#' @param numerator Marginal \eqn{P(C_t=0|C_{t-1}=0)}; omitted gives
#'   unstabilized weights.
#' @return Named list with `estimate`, `weights`, `unstabilized`,
#'   `mean_weight`, `max_weight`, `n`, `n_times`, `method`.
#' @references Robins (1993) Proc Biopharm Sect ASA 24-33.
#' @examples
#' Stbciw(matrix(c(0.9, 0.8, 0.95, 0.85), nrow = 2))
#' @export
Stbciw <- function(C, H = NULL, numerator = NULL) {
  D <- as.matrix(C); storage.mode(D) <- "double"
  n <- nrow(D); nt <- ncol(D)
  if (n == 0) stop("need at least one subject", call. = FALSE)
  if (any(D <= 0 | D > 1))
    stop("censoring probabilities must lie in (0, 1]", call. = FALSE)
  N <- if (is.null(numerator)) matrix(1, n, nt) else {
    M <- as.matrix(numerator); storage.mode(M) <- "double"
    if (nrow(M) != n || ncol(M) != nt)
      stop("numerator must match C", call. = FALSE)
    M
  }
  if (!is.null(H) && NROW(H) != n)
    stop("H must have one row per subject", call. = FALSE)
  den <- apply(D, 1, prod); num <- apply(N, 1, prod)
  w <- num / den
  list(estimate = sum(w) / n, weights = w, unstabilized = 1 / den,
       mean_weight = sum(w) / n, max_weight = max(w), n = n, n_times = nt,
       method = "stabilized censoring weights (Robins 1993)")
}

#' Augmented IPW estimator of the ATE
#'
#' Handbook eq. (23.4) p.512:
#' \deqn{\frac{1}{n}\sum_i \left\{ \frac{W_i(Y_i-m_{1i})}{p_{1i}} + m_{1i} - \frac{(1-W_i)(Y_i-m_{0i})}{p_{0i}} - m_{0i} \right\}}{mean( W(Y-m1)/p1 + m1 - (1-W)(Y-m0)/p0 - m0 )}
#' Consistent if either the outcome model or the propensity model is
#' correct.  With `m1 = m0 = 0` it collapses to the Horvitz-Thompson
#' IPW difference, which is a useful check on a call.
#'
#' @param y Outcomes.
#' @param D Treatment indicator, 0/1.
#' @param X Accepted for signature compatibility.
#' @param ml_outcome List or pair `(m1, m0)` of fitted outcome
#'   predictions.
#' @param ml_propensity Fitted \eqn{P(D=1|X)}, strictly in (0, 1).
#' @return Named list with `estimate`, `se`, `ci_lower`, `ci_upper`,
#'   `influence`, `ipw`, `plugin`, `n`, `method`.
#' @references Robins, Rotnitzky & Zhao (1994) JASA 89:846-866.
#' @examples
#' Eaiprl(c(1, 0, 1, 0), c(1, 0, 1, 0),
#'        ml_outcome = list(rep(0, 4), rep(0, 4)),
#'        ml_propensity = rep(0.5, 4))
#' @export
Eaiprl <- function(y, D, X = NULL, ml_outcome = NULL,
                   ml_propensity = NULL) {
  ys <- as.numeric(y); dd <- as.numeric(D); n <- length(ys)
  if (n == 0) stop("need at least one observation", call. = FALSE)
  if (length(dd) != n) stop("y and D must have the same length", call. = FALSE)
  if (is.null(ml_outcome) || is.null(ml_propensity))
    stop("ml_outcome (m1, m0) and ml_propensity are required", call. = FALSE)
  m1 <- as.numeric(ml_outcome[[1]]); m0 <- as.numeric(ml_outcome[[2]])
  e <- as.numeric(ml_propensity)
  if (length(m1) != n || length(m0) != n || length(e) != n)
    stop("nuisance predictions must have length n", call. = FALSE)
  if (any(e <= 0 | e >= 1))
    stop("propensities must lie strictly in (0, 1)", call. = FALSE)
  inf <- dd * (ys - m1) / e + m1 - (1 - dd) * (ys - m0) / (1 - e) - m0
  est <- sum(inf) / n
  se <- if (n > 1) sqrt(sum((inf - est)^2) / (n * n)) else NaN
  z <- 1.959963984540054
  ipw <- sum(dd * ys / e - (1 - dd) * ys / (1 - e)) / n
  list(estimate = est, se = se, ci_lower = est - z * se,
       ci_upper = est + z * se, influence = inf, ipw = ipw,
       plugin = sum(m1 - m0) / n, n = n,
       method = "augmented IPW ATE (Robins, Rotnitzky & Zhao 1994)")
}

#' Targeted maximum likelihood estimator of the ATE
#'
#' Clever covariates (Gruber & van der Laan 2012, eqs 2-3)
#' \eqn{H_1^*=I(A=1)/g(1|W)}, \eqn{H_0^*=I(A=0)/g(0|W)}, and the
#' fluctuation \eqn{logit(Q^*) = logit(Q) + \epsilon_0 H_0^* + \epsilon_1 H_1^*}
#' fitted by logistic regression of Y on the clever covariates with
#' offset \eqn{logit(Q)}.  `y` must be bounded in \[0, 1\] so the
#' fluctuation stays inside the model space.  When the initial fit
#' already solves the score, epsilon is 0 and the estimate equals
#' `plugin`.
#'
#' @param y Outcomes in \[0, 1\].
#' @param T Treatment indicator, 0/1.
#' @param ps Propensity \eqn{g(1|W)}, strictly in (0, 1).
#' @param Q1,Q0 Initial outcome fits under treatment and control.
#' @param n_iter Fixed Newton steps for the fluctuation.
#' @return Named list with `estimate`, `ATE_TMLE`, `IF`, `se`,
#'   `ci_lower`, `ci_upper`, `epsilon`, `EY1`, `EY0`, `plugin`, `n`,
#'   `method`.
#' @references van der Laan & Rubin (2006) Int J Biostat 2(1):11.
#' @examples
#' Caustmle(c(0.8, 0.3, 0.8, 0.3), c(1, 0, 1, 0), rep(0.5, 4),
#'          rep(0.8, 4), rep(0.3, 4))
#' @export
Caustmle <- function(y, T, ps, Q1, Q0, n_iter = 100L) {
  ys <- as.numeric(y); tt <- as.numeric(T); g <- as.numeric(ps)
  q1 <- as.numeric(Q1); q0 <- as.numeric(Q0); n <- length(ys)
  if (n == 0 || length(tt) != n || length(g) != n ||
      length(q1) != n || length(q0) != n)
    stop("all inputs must be non-empty and the same length", call. = FALSE)
  if (any(g <= 0 | g >= 1))
    stop("propensities must lie strictly in (0, 1)", call. = FALSE)
  if (any(ys < 0 | ys > 1))
    stop("y must be bounded in [0, 1] for the fluctuation", call. = FALSE)
  LO <- 1e-12
  lg <- function(p) { p <- pmin(pmax(p, LO), 1 - LO); log(p / (1 - p)) }
  ex <- function(z) ifelse(z >= 0, 1 / (1 + exp(-z)), exp(z) / (1 + exp(z)))
  h1 <- tt / g
  h0 <- (1 - tt) / (1 - g)
  qa <- ifelse(tt == 1, q1, q0)
  off <- lg(qa)
  e0 <- 0; e1 <- 0
  for (it in seq_len(as.integer(n_iter))) {
    mu <- ex(off + e0 * h0 + e1 * h1)
    r <- ys - mu; w <- mu * (1 - mu)
    s0 <- sum(h0 * r); s1 <- sum(h1 * r)
    a00 <- sum(w * h0 * h0); a01 <- sum(w * h0 * h1); a11 <- sum(w * h1 * h1)
    det <- a00 * a11 - a01 * a01
    if (abs(det) < 1e-14) break
    e0 <- e0 + (a11 * s0 - a01 * s1) / det
    e1 <- e1 + (a00 * s1 - a01 * s0) / det
  }
  q1s <- ex(lg(q1) + e1 / g)
  q0s <- ex(lg(q0) + e0 / (1 - g))
  ey1 <- sum(q1s) / n; ey0 <- sum(q0s) / n
  ate <- ey1 - ey0
  ic <- h1 * (ys - q1s) - h0 * (ys - q0s) + (q1s - q0s) - ate
  se <- sqrt(sum(ic * ic) / (n * n))
  z <- 1.959963984540054
  list(estimate = ate, ATE_TMLE = ate, IF = ic, se = se,
       ci_lower = ate - z * se, ci_upper = ate + z * se,
       epsilon = c(e0, e1), EY1 = ey1, EY0 = ey0,
       plugin = sum(q1 - q0) / n, n = n,
       method = "TMLE of the ATE (van der Laan & Rubin 2006)")
}

#' Controlled direct effect at a fixed mediator value
#'
#' \deqn{CDE(m) = E\[Y(1,m)\] - E\[Y(0,m)\]}{CDE(m) = E\[Y(1,m)\] - E\[Y(0,m)\]}
#' The mediator is set for everybody, not left where it would have
#' fallen -- that is what separates this from the natural direct
#' effect.  Under \eqn{E\[Y|X,M\] = b_0 + b_X X + b_M M + b_{XM} XM} the
#' contrast is \eqn{b_X + b_{XM} m}, so with no interaction it does not
#' depend on m at all.
#'
#' @param Y Outcomes.
#' @param X Treatment, 0/1.
#' @param M Mediator.
#' @param m Value the mediator is set to.
#' @return Named list with `estimate`, `cde`, `intercept`, `beta_x`,
#'   `beta_m`, `interaction`, `se`, `m`, `n`, `method`.
#' @references Robins & Greenland (1992) Epidemiology 3:143-155.
#' @examples
#' Cde(c(1, 2, 3, 4, 5, 6), c(0, 1, 0, 1, 0, 1), c(1, 1, 2, 2, 3, 3), 2)
#' @export
Cde <- function(Y, X, M, m) {
  ys <- as.numeric(Y); xs <- as.numeric(X); ms <- as.numeric(M)
  n <- length(ys)
  if (length(xs) != n || length(ms) != n)
    stop("Y, X and M must have the same length", call. = FALSE)
  if (n < 5)
    stop("need at least five observations for four parameters", call. = FALSE)
  mv <- as.numeric(m)
  D <- cbind(1, xs, ms, xs * ms)
  XtX <- crossprod(D)
  b <- as.numeric(solve(XtX, crossprod(D, ys)))
  cdev <- b[2] + b[4] * mv
  resid <- ys - as.numeric(D %*% b)
  s2 <- sum(resid^2) / (n - 4)
  inv <- solve(XtX)
  vr <- s2 * (inv[2, 2] + mv * mv * inv[4, 4] + 2 * mv * inv[2, 4])
  list(estimate = cdev, cde = cdev, intercept = b[1], beta_x = b[2],
       beta_m = b[3], interaction = b[4],
       se = if (vr > 0) sqrt(vr) else 0, m = mv, n = n,
       method = "controlled direct effect (Robins & Greenland 1992)")
}

#' G-estimation of a linear structural nested mean model
#'
#' The blipped-down outcome \eqn{Y - \psi \sum_t A_t} must be
#' conditionally independent of treatment given history, giving
#' \deqn{\psi = \frac{\sum_i (A_i - \bar A_i) Y_i}{\sum_i (A_i - \bar A_i) A_i}}{psi = sum (A-Abar)Y / sum (A-Abar)A}
#' Solved in one step -- the linear case needs no grid search over psi.
#' Under randomization with the sample mean as the propensity, psi
#' equals the OLS slope of Y on cumulative treatment exactly, which is
#' why `ols_slope` is returned next to it.
#'
#' @param y Outcomes.
#' @param treatment_history Subjects by time.
#' @param covariate_history,time Accepted for signature compatibility.
#' @param propensity \eqn{E\[A|H\]} for cumulative treatment; sample mean
#'   if omitted (correct only under randomization).
#' @return Named list with `estimate`, `psi`, `se`, `ci_lower`,
#'   `ci_upper`, `ols_slope`, `residual_treatment`, `n`, `method`.
#' @references Robins (1994) Commun Stat 23:2379-2412.
#' @examples
#' Snmlin(c(2, 1, 4, 2.5), matrix(c(1, 0, 1, 0, 0, 0, 1, 1), nrow = 4))
#' @export
Snmlin <- function(y, treatment_history, covariate_history = NULL,
                   time = NULL, propensity = NULL) {
  ys <- as.numeric(y)
  A <- as.matrix(treatment_history); storage.mode(A) <- "double"
  n <- length(ys)
  if (nrow(A) != n || n == 0)
    stop("y and treatment_history must agree and be non-empty", call. = FALSE)
  a <- as.numeric(rowSums(A))
  ea <- if (is.null(propensity)) rep(sum(a) / n, n) else as.numeric(propensity)
  if (length(ea) != n) stop("propensity must have length n", call. = FALSE)
  r <- a - ea
  den <- sum(r * a)
  if (abs(den) < 1e-12)
    stop("no residual treatment variation to identify psi", call. = FALSE)
  psi <- sum(r * ys) / den
  u <- r * (ys - psi * a)
  se <- if (n > 1) sqrt(sum(u * u)) / abs(den) else NaN
  ab <- sum(a) / n; yb <- sum(ys) / n
  saa <- sum((a - ab)^2)
  ols <- if (saa > 0) sum((a - ab) * (ys - yb)) / saa else NaN
  z <- 1.959963984540054
  list(estimate = psi, psi = psi, se = se, ci_lower = psi - z * se,
       ci_upper = psi + z * se, ols_slope = ols, residual_treatment = r,
       n = n, method = "g-estimation of a linear SNMM (Robins 1994)")
}
