# SPDX-License-Identifier: AGPL-3.0-or-later
#' Bootstrap variance of the sample mean
#'
#' \deqn{\mathrm{Var}^* = \frac{1}{B}\sum_b (\bar{x}^*_b - \overline{\bar{x}^*})^2.}{Var*
#' = (1/B) sum_b (xbar*_b - mean of them)^2.}
#'
#' Efron, B. (1979), "Bootstrap methods: another look at the jackknife",
#' \emph{The Annals of Statistics} 7(1), 1-26, doi:10.1214/aos/1176344552,
#' p. 3, steps 1-3 and Eq. (2.8), read from the Project Euclid PDF rendered as
#' page images.  For the simplest case, F putting all its mass on 0 and 1,
#' Efron prints E*(xbar* - xbar) = 0 and Var*(xbar* - xbar) = xbar(1-xbar)/n;
#' that is the complete-enumeration answer this module reproduces exactly when
#' exhaustive is set, and it is the anchor the module is checked against.
#'
#' More generally the complete bootstrap variance of the mean is
#' sigma-hat^2/n with sigma-hat^2 = sum (x_i - xbar)^2 / n, the population
#' form -- which on 0/1 data is xbar(1 - xbar), so Eq. (2.8) is the special
#' case.
#'
#' Resampling is deterministic; see Btmult for the Halton construction and the
#' meaning of rng.
#'
#' @param x The sample.
#' @param B Replications when not enumerating.
#' @param rng Base offset of the Halton design.
#' @param exhaustive Enumerate all n^n resamples (n <= 6).
#' @return list: estimate, var_b, mean_b, grand_mean, B, n, exhaustive, method.
#' @keywords internal
#' @examples
#' Btvarm(c(1, 1, 0, 1, 0), exhaustive = TRUE)$estimate
#' @export
Btvarm <- function(x, B = 200L, rng = 2L, exhaustive = FALSE) {
  v <- .s03vec(x)
  n <- length(v)
  if (n == 0L) stop("boot_var_mean: x is empty")
  B <- as.integer(B)
  if (!exhaustive && B < 1L) stop("boot_var_mean: B must be at least 1")
  rng <- as.integer(rng)
  if (rng < 2L) stop("boot_var_mean: rng must be a base of at least 2")
  cs <- .bt_counts(n, B, rng, isTRUE(exhaustive))
  nb <- nrow(cs)
  mb <- numeric(nb)
  for (b in seq_len(nb)) {
    s <- 0
    for (i in seq_len(n)) s <- s + cs[b, i] * v[i]
    mb[b] <- s / n
  }
  mm <- sum(mb) / nb
  vv <- sum((mb - mm)^2) / nb
  list(estimate = vv, var_b = vv, mean_b = mb, grand_mean = mm, B = nb,
       n = n, exhaustive = isTRUE(exhaustive),
       method = "Bootstrap variance of the sample mean")
}
