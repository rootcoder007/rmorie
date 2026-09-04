# morie.fn -- function file (rootcoder007/morie)
# The sample average treatment effect.
#
# In a cluster randomized trial the units are almost never a simple
# random sample from a defined population: the target population is
# hypothetical or ill-defined, and units are chosen for logistical
# reasons. The **population** average treatment effect is then a
# parameter of a superpopulation nobody sampled from -- neither well
# defined nor easily interpretable.
#
# The **sample** effect is the mean difference in counterfactual
# outcomes for *the study units themselves*,
#
# .. math:: \mathrm{SATE} = \frac{1}{n}\sum_{i=1}^{n}
#           \big(Y_i(1) - Y_i(0)\big),
#
# which is interpretable without inventing a superpopulation, and is
# arguably the more relevant quantity when the units were not sampled
# from one.
#
# **It is not identifiable in finite samples**, and the chapter says so
# plainly: the counterfactuals are not both observed for any unit. What
# rescues it is that the TMLE for the *population* effect is consistent
# and asymptotically linear for the sample effect too -- the same point
# estimate serves both, and only the inference changes.
#
# **The inference changes in one specific way.** The influence curve for
# the population effect carries two pieces: the weighted residual term
# and the term :math:`\bar Q_1 - \bar Q_0 - \psi`, which is the
# variability of the *individual* effects across units. The sample
# effect conditions on those units, so that second piece drops:
#
# .. math:: IC^{S} \approx \Big(\frac{I(A=1)}{g}
#           - \frac{I(A=0)}{1-g}\Big)(Y - \bar Q_A).
#
# The resulting variance is therefore **smaller by the variance of the
# conditional effect**, exactly, and the estimator is asymptotically
# conservative for the sample effect. Where effect modification is
# present -- where individual effects genuinely differ -- that gap is
# large, and targeting the sample effect is where the precision and
# power come from. The anchor computes both influence curves on the
# same data and requires the difference to equal
# :math:`\mathrm{var}(\bar Q_1 - \bar Q_0)`.
#
# **Pair-matched trials** are handled by the same argument with the
# matched-pair structure entering the variance estimate.
#
# References
# ----------
# van der Laan, M. J. & Rose, S. (2018) *Targeted Learning in Data
# Science*, Springer, doi:10.1007/978-3-319-65304-4. Chap. 12 (in
# cluster randomized trials the units are not a simple random sample and
# the target population is hypothetical or ill-defined, so the PATE may
# be neither well defined nor easily interpretable; the SATE as the
# mean difference in counterfactual outcomes for the study units; that
# the SATE is not formally identifiable in finite samples but a TMLE
# for the population effect is consistent and asymptotically linear
# for it with an asymptotically conservative variance estimator; the
# conservative influence curve dropping the Q1 - Q0 - psi term; the
# extension to pair-matched trials; and the finding that with effect
# modification, targeting the sample effect yields the most precision
# and power).
#
# Balzer, L. B., Petersen, M. L. & van der Laan, M. J. (2016) "Targeted
# estimation and inference for the sample average treatment effect in
# trials with and without pair-matching", *Statistics in Medicine*
# 35(21), 3717-3732, doi:10.1002/sim.6965.
#
# Imbens, G. W. (2004) "Nonparametric estimation of average treatment
# effects under exogeneity: a review", *Review of Economics and
# Statistics* 86(1), 4-29, doi:10.1162/003465304323023651. The
# sample/population distinction.

#' .tlsate_check
#'
#' A step of the tlsate_native implementation. Called by
#' \code{.tlsate_pate_influence_curve}, \code{.tlsate_sate_influence_curve},
#' \code{.tlsate_sate_tmle} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A Coerced to numeric by the body, with \code{as.numeric}.
#' @param Y Coerced to numeric by the body, with \code{as.numeric}.
#' @param Q1 Coerced to numeric by the body, with \code{as.numeric}.
#' @param Q0 Coerced to numeric by the body, with \code{as.numeric}.
#' @param g Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{a}, \code{y}, \code{q1}, \code{q0}, \code{gg}, \code{n}.
#' @export
.tlsate_check <- function(A, Y, Q1, Q0, g) {
  a <- as.numeric(A)
  y <- as.numeric(Y)
  q1 <- as.numeric(Q1)
  q0 <- as.numeric(Q0)
  gg <- as.numeric(g)
  n <- length(a)
  if (!(length(y) == length(q1) && length(q1) == length(q0) &&
    length(q0) == length(gg) && length(gg) == n)) {
    stop("tlsate: the inputs differ in length")
  }
  if (any(gg <= 0.0 | gg >= 1.0)) {
    stop("tlsate: the treatment probability must lie strictly inside (0,1)")
  }
  list(a = a, y = y, q1 = q1, q0 = q0, gg = gg, n = n)
}

#' .tlsate_logit
#'
#' A step of the tlsate_native implementation. Called by \code{.tlsate_sate_tmle}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param p Passed to \code{pmax}.
#' @return A numeric value.
#' @export
#' @examples
#' res <- .tlsate_logit(p = 0.5)
#' res
.tlsate_logit <- function(p) {
  q <- pmin(pmax(p, 1e-9), 1 - 1e-9)
  log(q / (1 - q))
}

#' .tlsate_expit
#'
#' A step of the tlsate_native implementation. Called by \code{.tlsate_sate_tmle}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Numeric; combined arithmetically in the body.
#' @return The value of \code{ifelse}.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .tlsate_expit(x = x)
#' res
.tlsate_expit <- function(x) {
  ifelse(x > -700, 1.0 / (1.0 + exp(-x)), 0.0)
}

#' .tlsate_var
#'
#' A step of the tlsate_native implementation. Called by \code{.tlsate_variance_gap}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v A vector; its length is taken.
#' @return A numeric value.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .tlsate_var(v = x)
#' res
.tlsate_var <- function(v) {
  m <- sum(v) / length(v)
  sum((v - m)^2) / (length(v) - 1)
}

#' .tlsate_se
#'
#' A step of the tlsate_native implementation. Called by \code{.tlsate_sate_tmle}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v A vector; its length is taken.
#' @return A numeric value.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .tlsate_se(v = x)
#' res
.tlsate_se <- function(v) {
  m <- sum(v) / length(v)
  sqrt(sum((v - m)^2) / (length(v) - 1) / length(v))
}

#' .tlsate_pate_influence_curve
#'
#' A step of the tlsate_native implementation. Called by \code{.tlsate_sate_tmle},
#' \code{.tlsate_variance_gap}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A Passed to \code{.tlsate_check}.
#' @param Y Passed to \code{.tlsate_check}.
#' @param Q1 Passed to \code{.tlsate_check}.
#' @param Q0 Passed to \code{.tlsate_check}.
#' @param g Passed to \code{.tlsate_check}.
#' @param psi Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.tlsate_pate_influence_curve <- function(A, Y, Q1, Q0, g, psi) {
  d <- .tlsate_check(A, Y, Q1, Q0, g)
  a <- d$a
  y <- d$y
  q1 <- d$q1
  q0 <- d$q0
  gg <- d$gg
  n <- d$n
  psi <- as.numeric(psi)
  qa <- ifelse(a == 1.0, q1, q0)
  h <- a / gg - (1.0 - a) / (1.0 - gg)
  h * (y - qa) + q1 - q0 - psi
}

#' .tlsate_sate_influence_curve
#'
#' A step of the tlsate_native implementation. Called by \code{.tlsate_sate_tmle},
#' \code{.tlsate_variance_gap}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A Passed to \code{.tlsate_check}.
#' @param Y Passed to \code{.tlsate_check}.
#' @param Q1 Passed to \code{.tlsate_check}.
#' @param Q0 Passed to \code{.tlsate_check}.
#' @param g Passed to \code{.tlsate_check}.
#' @return A numeric value.
#' @export
.tlsate_sate_influence_curve <- function(A, Y, Q1, Q0, g) {
  d <- .tlsate_check(A, Y, Q1, Q0, g)
  a <- d$a
  y <- d$y
  q1 <- d$q1
  q0 <- d$q0
  gg <- d$gg
  n <- d$n
  qa <- ifelse(a == 1.0, q1, q0)
  h <- a / gg - (1.0 - a) / (1.0 - gg)
  h * (y - qa)
}

#' .tlsate_variance_gap
#'
#' A step of the tlsate_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A Passed to \code{.tlsate_check}.
#' @param Y Passed to \code{.tlsate_check}.
#' @param Q1 Passed to \code{.tlsate_check}.
#' @param Q0 Passed to \code{.tlsate_check}.
#' @param g Passed to \code{.tlsate_check}.
#' @param psi Passed to \code{.tlsate_pate_influence_curve}.
#' @return A list with \code{var_pate}, \code{var_sate}, \code{gap},
#' \code{var_conditional_effect}, \code{note}.
#' @export
.tlsate_variance_gap <- function(A, Y, Q1, Q0, g, psi) {
  d <- .tlsate_check(A, Y, Q1, Q0, g)
  n <- d$n
  icp <- .tlsate_pate_influence_curve(A, Y, Q1, Q0, g, psi)
  ics <- .tlsate_sate_influence_curve(A, Y, Q1, Q0, g)
  eff <- d$q1 - d$q0
  list(
    var_pate = .tlsate_var(icp),
    var_sate = .tlsate_var(ics),
    gap = .tlsate_var(icp) - .tlsate_var(ics),
    var_conditional_effect = .tlsate_var(eff),
    note = "the gap IS the variance of the conditional effect; with no effect modification it is zero"
  )
}

#' .tlsate_sate_tmle
#'
#' A step of the tlsate_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A Passed to \code{.tlsate_check}.
#' @param Y Passed to \code{.tlsate_check}.
#' @param Q1 Passed to \code{.tlsate_check}.
#' @param Q0 Passed to \code{.tlsate_check}.
#' @param g Passed to \code{.tlsate_check}.
#' @return A list with \code{estimate}, \code{psi}, \code{se_population},
#' \code{se_sample}, \code{ci_population}, \code{ci_sample}, \code{width_ratio},
#' \code{method}, \code{note}.
#' @export
.tlsate_sate_tmle <- function(A, Y, Q1, Q0, g) {
  d <- .tlsate_check(A, Y, Q1, Q0, g)
  a <- d$a
  y <- d$y
  q1 <- d$q1
  q0 <- d$q0
  gg <- d$gg
  n <- d$n

  H <- a / gg - (1.0 - a) / (1.0 - gg)
  qa <- ifelse(a == 1.0, q1, q0)
  off <- .tlsate_logit(qa)

  e <- 0.0
  for (iter in seq_len(60)) {
    p <- .tlsate_expit(off + e * H)
    gr <- sum(H * (y - p))
    he <- sum(H * H * p * (1 - p))
    if (he < 1e-12) break
    e <- e + gr / he
  }

  q1s <- .tlsate_expit(.tlsate_logit(q1) + e / gg)
  q0s <- .tlsate_expit(.tlsate_logit(q0) - e / (1 - gg))
  psi <- sum(q1s - q0s) / n

  icp <- .tlsate_pate_influence_curve(a, y, q1s, q0s, gg, psi)
  ics <- .tlsate_sate_influence_curve(a, y, q1s, q0s, gg)

  sp <- .tlsate_se(icp)
  ss <- .tlsate_se(ics)

  list(
    estimate = psi,
    psi = psi,
    se_population = sp,
    se_sample = ss,
    ci_population = c(psi - 1.96 * sp, psi + 1.96 * sp),
    ci_sample = c(psi - 1.96 * ss, psi + 1.96 * ss),
    width_ratio = if (sp > 0) ss / sp else NaN,
    method = "TMLE with sample-effect inference; van der Laan & Rose (2018) Chap. 12",
    note = "same point estimate; the SAMPLE interval is narrower by the variance of the conditional effect, and is asymptotically conservative for the SATE"
  )
}

#' .tlsate_paired_variance
#'
#' A step of the tlsate_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param pair_ids Coerced to character by the body, with \code{as.character}.
#' @param ic Coerced to numeric by the body, with \code{as.numeric}.
#' @return A list with \code{se}, \code{n_pairs}, \code{note}.
#' @export
.tlsate_paired_variance <- function(pair_ids, ic) {
  p <- as.character(pair_ids)
  v <- as.numeric(ic)
  if (length(p) != length(v)) {
    stop(sprintf(
      "tlsate: %d pair labels for %d influence values",
      length(p), length(v)
    ))
  }

  agg <- split(v, p)

  if (any(sapply(agg, length) != 2)) {
    stop("tlsate: every pair must contain exactly 2 units")
  }

  sums <- sapply(agg, sum) / 2.0
  m <- mean(sums)
  v_var <- sum((sums - m)^2) / (length(sums) - 1)

  list(
    se = sqrt(v_var / length(sums)),
    n_pairs = length(sums),
    note = "the PAIR is the independent unit"
  )
}

#' .tlsate_cheatsheet
#'
#' A step of the tlsate_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
#' @examples
#' res <- .tlsate_cheatsheet()
#' res
.tlsate_cheatsheet <- function() {
  paste0(
    "tlsate: in a cluster randomized trial the units are not samp",
    "led from any defined population, so the PATE is a parameter ",
    "of a superpopulation nobody drew from. The SATE -- the mean ",
    "counterfactual difference for THESE units -- is interpretabl",
    "e without inventing one. It is not identifiable in finite sa",
    "mples, but the SAME TMLE is consistent and asymptotically li",
    "near for it; only the influence curve changes, dropping Q1 -",
    " Q0 - psi. The variance falls by EXACTLY the variance of the",
    " conditional effect, so effect modification is where the pow",
    "er gain comes from."
  )
}

# compact alias per ledger/NAMING.md
morie_tlsate <- .tlsate_sate_tmle
