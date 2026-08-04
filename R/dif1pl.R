# SPDX-License-Identifier: AGPL-3.0-or-later
#' Mantel-Haenszel differential item functioning statistic
#'
#' Source: Holland, P. W. and Thayer, D. T. (1988), Differential item
#' performance and the Mantel-Haenszel procedure, in Wainer and Braun
#' (eds), Test Validity, 129-145, applying Mantel and Haenszel (1959),
#' JNCI 22, 719-748.  Neither chapter was obtainable here; the procedure
#' is quoted in its standard published form, which is exactly the
#' stratified 2x2 statistic base R implements in
#' \code{stats::mantelhaen.test} -- used as the independent anchor.
#'
#' Per matching stratum k, with the 2x2 table of item-right/item-wrong
#' by reference/focal group,
#' \code{E[A_k] = n1_k m1_k / T_k} and
#' \code{Var[A_k] = n1_k n2_k m1_k m0_k / (T_k^2 (T_k - 1))}, giving
#' \code{chi2 = (|sum A_k - sum E[A_k]| - 0.5)^2 / sum Var[A_k]} on one
#' degree of freedom, and
#' \code{alpha_MH = sum(A_k D_k/T_k) / sum(B_k C_k/T_k)} with the ETS
#' delta scale \code{-2.35 log(alpha_MH)}.  Strata with fewer than two
#' examinees, or with no variation in the item or in group membership,
#' carry no information and are dropped.
#'
#' @param y Response to the studied item, coded 0/1.
#' @param group Group membership; exactly two distinct values, the first
#'   encountered being the reference group.
#' @param item Optional matching variable (normally total score);
#'   examinees are stratified on its distinct values.  A single stratum
#'   is used when omitted.
#' @param correct Apply the 0.5 continuity correction.  Default TRUE.
#' @param reference Which value of \code{group} is the reference group.
#'   Defaults to the first value encountered.  \code{statistic} is
#'   unaffected, but \code{alpha_MH} inverts and \code{delta_MH} changes
#'   sign, so pass this explicitly whenever the direction of the DIF
#'   matters.
#' @return list: statistic, p_value, df, alpha_MH, delta_MH, sum_A,
#'   sum_E, sum_V, n_strata, n_used, n, method.
#' @examples
#' Difmh(c(1, 0, 1, 1, 0, 0, 1, 0), c("r", "r", "r", "r", "f", "f", "f", "f"))$statistic
#' @export
Difmh <- function(y, group, item = NULL, correct = TRUE, reference = NULL) {
  y <- as.numeric(y)
  n <- length(y)
  if (length(group) != n) stop("group must be the same length as y")
  if (!all(y == 0 | y == 1)) stop("y must be coded 0/1")
  levs <- unique(as.character(group))
  if (length(levs) != 2L) stop("group must have exactly 2 distinct values")
  ref <- if (is.null(reference)) {
    levs[1]
  } else if (as.character(reference) %in% levs) {
    as.character(reference)
  } else {
    stop("reference is not one of the two group values")
  }
  is_ref <- as.character(group) == ref
  strata <- if (is.null(item)) rep(0L, n) else item
  if (length(strata) != n) stop("item must be the same length as y")

  sum_a <- 0
  sum_e <- 0
  sum_v <- 0
  num <- 0
  den <- 0
  used <- 0L
  n_used <- 0L
  for (kk in unique(strata)) {
    idx <- which(strata == kk)
    T <- length(idx)
    if (T < 2) next
    A <- sum(is_ref[idx] & y[idx] == 1)
    B <- sum(is_ref[idx] & y[idx] == 0)
    Cc <- sum(!is_ref[idx] & y[idx] == 1)
    D <- sum(!is_ref[idx] & y[idx] == 0)
    n1 <- A + B
    n2 <- Cc + D
    m1 <- A + Cc
    m0 <- B + D
    if (n1 == 0 || n2 == 0 || m1 == 0 || m0 == 0) next
    sum_a <- sum_a + A
    sum_e <- sum_e + n1 * m1 / T
    sum_v <- sum_v + n1 * n2 * m1 * m0 / (T^2 * (T - 1))
    num <- num + A * D / T
    den <- den + B * Cc / T
    used <- used + 1L
    n_used <- n_used + T
  }
  if (used == 0L || sum_v <= 0) stop("no stratum carries information about DIF")

  d <- abs(sum_a - sum_e)
  if (correct) d <- max(0, d - 0.5)
  stat <- d^2 / sum_v
  alpha_mh <- if (den > 0) num / den else Inf
  delta_mh <- if (is.finite(alpha_mh) && alpha_mh > 0) -2.35 * log(alpha_mh) else NaN
  list(
    statistic = stat, p_value = stats::pchisq(stat, 1, lower.tail = FALSE),
    df = 1L, alpha_MH = alpha_mh, delta_MH = delta_mh,
    sum_A = sum_a, sum_E = sum_e, sum_V = sum_v,
    n_strata = used, n_used = n_used, n = n,
    method = paste(
      "Mantel-Haenszel DIF chi-square",
      "(Holland and Thayer 1988; Mantel and Haenszel 1959)"
    )
  )
}
