# SPDX-License-Identifier: AGPL-3.0-or-later
#
# R mirror of morie.fn.{sobel,tcc,tinfo,lordzs,macn,taroni,shscl} --
# slice k05, tranche 2.
#
# Every one of these Python modules previously carried a verbatim
# one-sample Kolmogorov-Smirnov test against a fitted normal. The
# bodies were deleted and rewritten from the primary sources named in
# each function's @references.
#
# Anchors, so agreement between the arms is not the only evidence:
#   Sobel      delta-method SE reproduced by hand
#   IRT        2PL information collapses to a^2 P Q exactly; the 3PL
#              form matches Birnbaum's published expression exactly;
#              guessing shifts the information peak ABOVE b
#   TCC        floor is sum(c) and ceiling is n, strictly increasing
#   Lord       the 1PL branch reduces to (v1-v2)^2/(s1^2+s2^2)
#   Cochran    identical effects give Q == 0; k = 2 gives
#              (y1-y2)^2/(v1+v2)
#   Tarone-Ware  weight = 1 reproduces morie_logrank_test exactly, and
#              sqrt(n) lands between the log-rank and Gehan statistics
#   Schoenfeld mean(scaled) == beta EXACTLY, by the score equations

#' Sobel test for an indirect (mediated) effect
#'
#' The delta method applied to f(a,b) = ab, whose gradient is (b, a),
#' gives Var(ab) ~ b^2 sa^2 + a^2 sb^2 and z = ab / sqrt(that). The
#' exact variance of a product of independent normals carries a third
#' term and the classical variants differ only in its sign: "aroian"
#' adds sa^2 sb^2 (the exact variance), "goodman" subtracts it (the
#' unbiased estimator, which can go negative -- then no z exists and
#' this errors rather than returning a fabricated number).
#'
#' The p-value assumes ab is normal. It is not: a product of normals is
#' skewed and heavy-tailed, so the test is under-powered and its
#' interval is symmetric when the truth is not. That is a property of
#' the method, not of this implementation.
#'
#' @param a,b path coefficients (X->M and M->Y).
#' @param se_a,se_b their standard errors.
#' @param variant one of "sobel", "aroian", "goodman".
#' @return list: statistic (z), pvalue, indirect_effect, se, variant,
#'   ci_lower, ci_upper, method.
#' @references Sobel, M. E. (1982), \emph{Sociological Methodology} 13,
#'   290-312; Aroian, L. A. (1947), \emph{Ann. Math. Statist.} 18,
#'   265-271; Goodman, L. A. (1960), \emph{JASA} 55, 708-713.
#' @examples
#' morie_sobel_test(0.5, 0.4, 0.1, 0.08)$statistic
#' @export
morie_sobel_test <- function(a, b, se_a, se_b, variant = "sobel") {
  if (!variant %in% c("sobel", "aroian", "goodman"))
    stop("variant must be one of sobel, aroian, goodman", call. = FALSE)
  if (se_a < 0 || se_b < 0) stop("standard errors must be non-negative.", call. = FALSE)
  va <- se_a^2
  vb <- se_b^2
  vv <- b * b * va + a * a * vb
  if (variant == "aroian") vv <- vv + va * vb
  if (variant == "goodman") vv <- vv - va * vb
  if (vv <= 0)
    stop(sprintf("non-positive variance for the indirect effect (variant=%s); no z statistic exists.",
                 variant), call. = FALSE)
  se <- sqrt(vv)
  est <- a * b
  z <- est / se
  list(statistic = z, pvalue = 2 * stats::pnorm(abs(z), lower.tail = FALSE),
       indirect_effect = est, se = se, variant = variant,
       a = a, b = b, se_a = se_a, se_b = se_b,
       ci_lower = est - 1.959963984540054 * se,
       ci_upper = est + 1.959963984540054 * se,
       method = sprintf("Sobel (1982) delta-method test of a*b (%s variance)", variant))
}

#' .morie_k05_item_params
#'
#' A step of the k05_tranche2 implementation. Called by \code{morie_tcc}, \code{morie_test_information}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a Passed to \code{rep_to}.
#' @param b Coerced to numeric by the body, with \code{as.numeric}.
#' @param c Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @param upper Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @return A list with \code{a}, \code{b}, \code{c}, \code{u}, \code{n}.
#' @export
.morie_k05_item_params <- function(a, b, c = NULL, upper = NULL) {
  bb <- as.numeric(b)
  n <- length(bb)
  if (n == 0L) stop("need at least one item.", call. = FALSE)
  rep_to <- function(x, nm) {
    v <- as.numeric(x)
    if (length(v) == 1L) v <- rep(v, n)
    if (length(v) != n)
      stop(sprintf("%s must have one entry per item (got %d, expected %d)", nm, length(v), n),
           call. = FALSE)
    v
  }
  aa <- rep_to(a, "a")
  cc <- if (is.null(c)) rep(0, n) else rep_to(c, "c")
  uu <- if (is.null(upper)) rep(1, n) else rep_to(upper, "upper")
  if (any(!(cc >= 0 & cc < uu & uu <= 1)))
    stop("need 0 <= c_i < upper_i <= 1 for every item.", call. = FALSE)
  list(a = aa, b = bb, c = cc, u = uu, n = n)
}

#' Branch on the sign so exp never overflows for large |z|
#'
#' A step of the k05_tranche2 implementation. Called by \code{.morie_k05_info}, \code{morie_tcc}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param theta Numeric; combined arithmetically in the body.
#' @param a Numeric; combined arithmetically in the body.
#' @param b Numeric; combined arithmetically in the body.
#' @param c Numeric; combined arithmetically in the body.
#' @param u Numeric; combined arithmetically in the body.
#' @param D Numeric; combined arithmetically in the body. Defaults to \code{1}.
#' @return A numeric value.
#' @export
.morie_k05_prob <- function(theta, a, b, c, u, D = 1) {
  # branch on the sign so exp never overflows for large |z|
  z <- D * a * (theta - b)
  p2 <- ifelse(z >= 0, 1 / (1 + exp(-z)), exp(z) / (1 + exp(z)))
  c + (u - c) * p2
}

#' General two-category Fisher information I = (P\')^2/(PQ);
#' substituting
#'
#' the 4PL P and using P-c = (u-c)P*, u-P = (u-c)Q* gives this form,
#' which collapses to D^2 a^2 P Q when c = 0, u = 1.
#'
#' @param theta Passed to \code{.morie_k05_prob}.
#' @param a Numeric; combined arithmetically in the body.
#' @param b Passed to \code{.morie_k05_prob}.
#' @param c Numeric; combined arithmetically in the body.
#' @param u Numeric; combined arithmetically in the body.
#' @param D Numeric; combined arithmetically in the body. Defaults to \code{1}.
#' @return A numeric value.
#' @export
.morie_k05_info <- function(theta, a, b, c, u, D = 1) {
  # general two-category Fisher information I = (P')^2/(PQ); substituting
  # the 4PL P and using P-c = (u-c)P*, u-P = (u-c)Q* gives this form,
  # which collapses to D^2 a^2 P Q when c = 0, u = 1.
  p <- .morie_k05_prob(theta, a, b, c, u, D)
  q <- 1 - p
  if (p <= 0 || q <= 0) return(0)
  num <- (p - c) * (u - p)
  (D * a)^2 * num * num / ((u - c)^2 * p * q)
}

#' Test characteristic curve
#'
#' T(theta) = sum_i P_i(theta), the expected number-correct score. It is
#' the IRT true score and is strictly increasing in theta, so it can be
#' inverted to map a raw score onto an ability -- the basis of
#' true-score equating. Its floor is sum(c), not zero: with guessing an
#' examinee at theta = -Inf still expects sum(c) items right.
#'
#' @param theta ability value(s).
#' @param a,b item discriminations and difficulties; scalar a broadcasts.
#' @param c lower asymptotes (guessing); default 0, giving the 2PL.
#' @param upper upper asymptotes; default 1.
#' @param D scaling constant; 1 logistic, 1.702 normal-ogive.
#' @return list: tcc, theta, n_items, floor, ceiling, D, method.
#' @references Lord, F. M. (1980), \emph{Applications of Item Response
#'   Theory to Practical Testing Problems}, Erlbaum, ch. 4.
#' @examples
#' morie_tcc(0, c(1.2, 0.8), c(-0.5, 0.3))$tcc
#' @export
morie_tcc <- function(theta, a, b, c = NULL, upper = NULL, D = 1) {
  ip <- .morie_k05_item_params(a, b, c, upper)
  th <- as.numeric(theta)
  vals <- vapply(th, function(t)
    sum(vapply(seq_len(ip$n), function(i)
      .morie_k05_prob(t, ip$a[i], ip$b[i], ip$c[i], ip$u[i], D), numeric(1))), numeric(1))
  list(tcc = vals, theta = th, n_items = ip$n, floor = sum(ip$c),
       ceiling = sum(ip$u), D = D,
       method = "Test characteristic curve, sum of item response functions")
}

#' Test information function
#'
#' I(theta) = sum_i I_i(theta). Item information is ADDITIVE -- the
#' property that makes IRT test assembly possible, since each item's
#' contribution is evaluable without reference to the rest of the form.
#' For a 2PL item it reduces to D^2 a^2 P Q, peaking at theta = b;
#' guessing lowers the peak AND shifts it above b, because a correct
#' response carries less information when it may have been a guess. The
#' conditional standard error follows as SEM = 1/sqrt(I).
#'
#' @param theta ability value(s).
#' @param a,b item discriminations and difficulties.
#' @param c lower asymptotes; default 0.
#' @param upper upper asymptotes; default 1.
#' @param D scaling constant.
#' @return list: information, sem, item_information, theta, n_items, D, method.
#' @references Lord, F. M. (1980), ch. 5; Birnbaum, A. (1968), in Lord &
#'   Novick, \emph{Statistical Theories of Mental Test Scores}, ch. 17.
#' @examples
#' morie_test_information(0, c(1.2, 0.8), c(-0.5, 0.3))$information
#' @export
morie_test_information <- function(theta, a, b, c = NULL, upper = NULL, D = 1) {
  ip <- .morie_k05_item_params(a, b, c, upper)
  th <- as.numeric(theta)
  per <- lapply(th, function(t)
    vapply(seq_len(ip$n), function(i)
      .morie_k05_info(t, ip$a[i], ip$b[i], ip$c[i], ip$u[i], D), numeric(1)))
  tot <- vapply(per, sum, numeric(1))
  list(information = tot, sem = ifelse(tot > 0, 1 / sqrt(tot), Inf),
       item_information = per, theta = th, n_items = ip$n, D = D,
       method = "Test information function, sum of item informations")
}

#' Lord's chi-square test for differential item functioning
#'
#' chi2 = (vR - vF)' (SigR + SigF)^-1 (vR - vF) on p df, p being the
#' number of parameters compared. The covariances ADD because the two
#' groups are independent samples.
#'
#' This presumes the estimates are already on a common metric. IRT
#' parameters are identified only up to a linear transformation of
#' theta, so without linking a significant result may be reporting the
#' scale difference rather than DIF. No arithmetic here can detect that.
#'
#' @param b_R,b_F item parameter estimates in reference and focal groups.
#' @param V_R covariance of b_R, or the already-summed covariance if
#'   V_F is omitted.
#' @param V_F covariance of b_F, optional.
#' @return list: statistic, pvalue, df, difference, method.
#' @references Lord, F. M. (1980), ch. 14. Cross-checked against
#'   \code{LordChi2} in the difR package.
#' @examples
#' morie_lord_chisq(0.4, 0.1, matrix(0.04), matrix(0.09))$statistic
#' @export
morie_lord_chisq <- function(b_R, b_F, V_R, V_F = NULL) {
  vr <- as.numeric(b_R)
  vf <- as.numeric(b_F)
  if (length(vr) != length(vf)) stop("b_R and b_F must have the same length.", call. = FALSE)
  p <- length(vr)
  d <- vr - vf
  S <- matrix(as.numeric(as.matrix(V_R)), nrow = p)
  if (!is.null(V_F)) {
    S2 <- matrix(as.numeric(as.matrix(V_F)), nrow = p)
    if (!identical(dim(S2), dim(S))) stop("V_R and V_F must have the same shape.", call. = FALSE)
    S <- S + S2
  }
  if (!identical(dim(S), c(p, p)))
    stop(sprintf("covariance must be %d x %d for %d parameters.", p, p, p), call. = FALSE)
  stat <- as.numeric(crossprod(d, solve(S, d)))
  if (stat < 0)
    stop("negative quadratic form: the summed covariance is not positive definite, so no chi-square statistic exists.",
         call. = FALSE)
  list(statistic = stat, pvalue = stats::pchisq(stat, p, lower.tail = FALSE),
       df = p, difference = d,
       method = "Lord (1980) chi-square test of item parameter equality")
}

# Cochran's Q: the R arm already lives in aaa_macn.R as
# morie_ma_cochran_q, written by another agent against this same Python
# module while this slice was in flight. Its arithmetic, field names and
# method string are identical to what this file would have defined, so
# rather than ship a second copy that can silently drift, morie_cochran_q
# is an alias of it. See aaa_macn.R for the references and the derivation.
#' Cochran\'s Q: the R arm already lives in aaa_macn.R as
#'
#' morie_ma_cochran_q, written by another agent against this same Python
#' module while this slice was in flight. Its arithmetic, field names
#' and method string are identical to what this file would have defined,
#' so rather than ship a second copy that can silently drift,
#' morie_cochran_q is an alias of it. See aaa_macn.R for the references
#' and the derivation.
#'
#' @param yi Passed to \code{morie_ma_cochran_q}.
#' @param vi Passed to \code{morie_ma_cochran_q}.
#' @return The value of \code{morie_ma_cochran_q}.
#' @export
morie_cochran_q <- function(yi, vi) morie_ma_cochran_q(yi, vi)

#' Tarone-Ware and the weighted log-rank family
#'
#' At each event time with n_j at risk, n_1j in group 1, d_j deaths of
#' which d_1j in group 1, e_1j = d_j n_1j / n_j and
#' V_j = d_j (n_j - d_j) n_1j (n_j - n_1j) / (n_j^2 (n_j - 1)). The
#' family is chi2 = \[sum w_j (d_1j - e_1j)\]^2 / sum w_j^2 V_j on 1 df,
#' with w_j = 1 (log-rank), n_j (Gehan), sqrt(n_j) (Tarone-Ware) or
#' S(t_j) (Peto). Tarone-Ware's sqrt(n_j) sits deliberately between the
#' log-rank, most powerful under proportional hazards, and Gehan, most
#' powerful under early separation. Choosing the weight AFTER seeing
#' the curves invalidates the p-value.
#'
#' @param time follow-up times.
#' @param event 1 event, 0 right-censored.
#' @param group exactly two distinct labels.
#' @param weight one of "tarone-ware", "logrank", "gehan", "peto".
#' @return list: statistic, pvalue, df, observed, expected, score,
#'   variance, n_events, n_event_times, groups, weight, method.
#' @references Tarone, R. E. & Ware, J. (1977), \emph{Biometrika} 64(1),
#'   156-160. Weights cross-checked against \code{comp.ten} in survMisc.
#' @examples
#' morie_tarone_ware(c(1, 2, 3, 4, 5, 6), rep(1, 6), c(0, 1, 0, 1, 0, 1))$statistic
#' @export
morie_tarone_ware <- function(time, event, group, weight = "tarone-ware") {
  if (!weight %in% c("tarone-ware", "logrank", "gehan", "peto"))
    stop("weight must be one of tarone-ware, logrank, gehan, peto", call. = FALSE)
  t <- as.numeric(time)
  e <- as.numeric(event)
  g <- as.vector(group)
  if (length(t) != length(e) || length(t) != length(g))
    stop("time, event and group must have the same length.", call. = FALSE)
  labs <- unique(g)
  if (length(labs) != 2L)
    stop(sprintf("need exactly 2 groups; got %d.", length(labs)), call. = FALSE)
  a <- labs[1]
  ut <- sort(unique(t[e == 1]))
  if (length(ut) == 0L) stop("no events.", call. = FALSE)
  n <- vapply(ut, function(tt) sum(t >= tt), numeric(1))
  n1 <- vapply(ut, function(tt) sum(t >= tt & g == a), numeric(1))
  d <- vapply(ut, function(tt) sum(t == tt & e == 1), numeric(1))
  d1 <- vapply(ut, function(tt) sum(t == tt & e == 1 & g == a), numeric(1))
  # Peto weight is the left-continuous modified KM estimate
  peto <- numeric(length(ut))
  s <- 1
  for (j in seq_along(ut)) { peto[j] <- s
  s <- s * (1 - d[j] / (n[j] + 1)) }
  w <- switch(weight, "logrank" = rep(1, length(ut)), "gehan" = n,
              "tarone-ware" = sqrt(n), "peto" = peto)
  keep <- n > 1
  e1 <- d * n1 / n
  vj <- d * (n - d) * n1 * (n - n1) / (n^2 * (n - 1))
  num <- sum((w * (d1 - e1))[keep])
  den <- sum((w^2 * vj)[keep])
  if (den <= 0) stop("zero variance; the groups cannot be compared.", call. = FALSE)
  stat <- num^2 / den
  list(statistic = stat, pvalue = stats::pchisq(stat, 1, lower.tail = FALSE),
       df = 1, observed = sum(d1[keep]), expected = sum(e1[keep]),
       score = num, variance = den, n_events = sum(d),
       n_event_times = length(ut), groups = as.character(labs), weight = weight,
       method = "Tarone-Ware (1977) family of weighted log-rank tests")
}

#' .morie_k05_schoenfeld
#'
#' A step of the k05_tranche2 implementation. Called by \code{morie_scaled_schoenfeld}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param t A vector; its length is taken and its elements indexed.
#' @param e A vector; indexed elementwise.
#' @param X A matrix; indexed by row and column.
#' @param beta A matrix; passed to \code{\%*\%}.
#' @return A list with \code{times}, \code{res}, \code{var}.
#' @export
.morie_k05_schoenfeld <- function(t, e, X, beta) {
  n <- length(t)
  p <- ncol(X)
  idx <- order(t)
  times <- numeric(0)
  res <- list()
  var <- list()
  for (i in idx) {
    if (e[i] != 1) next
    risk <- which(t >= t[i])
    w <- exp(as.numeric(X[risk, , drop = FALSE] %*% beta))
    sw <- sum(w)
    xbar <- as.numeric(crossprod(X[risk, , drop = FALSE], w)) / sw
    dev <- sweep(X[risk, , drop = FALSE], 2, xbar, "-")
    # weighted covariance over the risk set: the per-event-time
    # contribution to the information, and exactly Var(s_j) under H0
    V <- crossprod(dev * sqrt(w)) / sw
    times <- c(times, t[i])
    res[[length(res) + 1L]] <- as.numeric(X[i, ]) - xbar
    var[[length(var) + 1L]] <- matrix(V, nrow = p)
  }
  list(times = times, res = res, var = var)
}

#' .morie_k05_gtime
#'
#' A step of the k05_tranche2 implementation. Called by \code{morie_scaled_schoenfeld}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param times A vector; its length is taken and its elements indexed.
#' @param e_times Passed to \code{>=}.
#' @param how One of \code{"identity"}, \code{"log"}, \code{"rank"}.
#' @return The value of \code{out}, as built in the body.
#' @export
.morie_k05_gtime <- function(times, e_times, how) {
  if (how == "identity") return(times)
  if (how == "log") return(log(times))
  if (how == "rank") return(as.numeric(rank(times, ties.method = "first")))
  surv <- 1
  out <- numeric(length(times))
  for (j in seq_along(times)) {
    tt <- times[j]
    out[j] <- 1 - surv
    nr <- sum(e_times >= tt)
    dd <- sum(e_times == tt)
    if (nr > 0) surv <- surv * (1 - dd / nr)
  }
  out
}

#' Scaled Schoenfeld residuals and the Grambsch-Therneau PH test
#'
#' The raw Schoenfeld residual is the failing subject's covariate minus
#' the hazard-weighted risk-set mean. Rescaling by the information
#' turns it into an estimate of the coefficient AT THAT TIME:
#' s* = beta + d Var(beta) s, with E\[s*_j\] ~ beta(t_j), so plotting s*
#' against g(t) shows the coefficient's trajectory and proportional
#' hazards is the hypothesis that it is flat.
#'
#' The test is the score test of beta(t) = beta + theta g(t) at
#' theta = 0. Since Var(s_j) = V_j, the risk-set covariance at t_j, both
#' the score U = sum (g_j - gbar) s_j and its variance
#' sum (g_j - gbar)^2 V_j are closed form -- no optimisation, so the
#' arms agree exactly rather than to an optimiser's tolerance.
#'
#' A significant result does not mean the covariate is unimportant; it
#' means a SINGLE hazard ratio is the wrong summary. The fix is
#' stratification or a time-varying coefficient, not deletion.
#'
#' Note mean(scaled) == beta exactly: the score equations make the raw
#' residuals sum to zero at the MLE, so rescaling leaves the mean at
#' beta. That identity is a cheap check that a fit converged.
#'
#' @param time follow-up times.
#' @param event 1 event, 0 right-censored.
#' @param X covariate matrix, n x p.
#' @param transform time transform g: "km", "rank", "identity", "log".
#' @return list: scaled, residuals, times, gtime, beta, vcov, statistic,
#'   pvalue, global_statistic, global_pvalue, df, n_events, transform, method.
#' @references Schoenfeld, D. (1982), \emph{Biometrika} 69(1), 239-241;
#'   Grambsch, P. M. & Therneau, T. M. (1994), \emph{Biometrika} 81(3),
#'   515-526.
#' @examples
#' \donttest{
#' set.seed(1); n <- 60; x <- matrix(stats::rnorm(n), ncol = 1)
#' morie_scaled_schoenfeld(stats::rexp(n), rep(1, n), x)$df
#' }
#' @export
morie_scaled_schoenfeld <- function(time, event, X, transform = "km") {
  if (!transform %in% c("km", "rank", "identity", "log"))
    stop("transform must be one of km, rank, identity, log", call. = FALSE)
  t <- as.numeric(time)
  e <- as.numeric(event)
  X <- as.matrix(X)
  if (length(t) != length(e) || length(t) != nrow(X))
    stop("time, event and X must agree in length.", call. = FALSE)
  p <- ncol(X)
  fit <- morie_cox_ph(t, e, X)
  beta <- as.numeric(fit$coef)
  Vb <- matrix(as.numeric(fit$vcov), nrow = p)
  sc <- .morie_k05_schoenfeld(t, e, X, beta)
  d <- length(sc$times)
  if (d < 3L) stop("need at least 3 events.", call. = FALSE)
  g <- .morie_k05_gtime(sc$times, t[e == 1], transform)
  gc <- g - mean(g)
  scaled <- lapply(seq_len(d), function(j) as.numeric(beta + d * (Vb %*% sc$res[[j]])))
  # vapply gives a vector when p == 1, so re-shape before summing rows
  U <- rowSums(matrix(vapply(seq_len(d), function(j) gc[j] * sc$res[[j]], numeric(p)), nrow = p))
  VU <- matrix(0, p, p)
  for (j in seq_len(d)) VU <- VU + gc[j]^2 * sc$var[[j]]
  stat <- vapply(seq_len(p), function(k)
    if (VU[k, k] > 0) U[k]^2 / VU[k, k] else NaN, numeric(1))
  gs <- tryCatch(as.numeric(crossprod(U, solve(VU, U))), error = function(...) NaN)
  list(scaled = scaled, residuals = sc$res, times = sc$times, gtime = g,
       beta = beta, vcov = Vb, statistic = stat,
       pvalue = stats::pchisq(stat, 1, lower.tail = FALSE),
       global_statistic = gs,
       global_pvalue = if (is.finite(gs)) stats::pchisq(gs, p, lower.tail = FALSE) else NaN,
       df = p, n_events = d, transform = transform,
       method = "Grambsch-Therneau (1994) scaled Schoenfeld residuals and PH score test")
}
