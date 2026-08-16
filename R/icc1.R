# SPDX-License-Identifier: AGPL-3.0-or-later
#' ICC(1) one-way random-effects intraclass correlation
#'
#' Shrout, P. E. and Fleiss, J. L. (1979), "Intraclass correlations: uses in
#' assessing rater reliability", \emph{Psychological Bulletin} 86(2), 420-428,
#' doi:10.1037/0033-2909.86.2.420, is the primary source for the three forms.
#' That paper is closed access with no open copy in any repository (checked
#' against Unpaywall, which reports oa_status "closed" and an empty
#' oa_locations list), so the arithmetic below was read from a source that
#' reproduces it: Hedderich, J., Sachs, L. and Reynarowych, Z., \emph{Applied
#' Statistics: Methods Using R}, Springer, Section 6.16 "Agreement and
#' Precision of Measurements", pp. 427-428, whose worked R function is
#' labelled "ANOVA according to Shrout-Fleiss" and gives
#'
#' \deqn{SS_t = \sum x^2 - T^2/(nk), \quad SS_a = \sum(\mathrm{row\ totals})^2/k - T^2/(nk),}{SS_t = sum x^2 - T^2/(nk),  SS_a = sum(row totals^2)/k - T^2/(nk),}
#' \deqn{BMS = SS_a/(n-1), \quad WMS = (SS_t - SS_a)/(n(k-1)),}{BMS = SS_a/(n-1),  WMS = (SS_t - SS_a)/(n(k-1)),}
#' \deqn{ICC(1,1) = (BMS - WMS)/(BMS + (k-1)WMS),}{ICC(1,1) = (BMS - WMS)/(BMS + (k-1) WMS),}
#'
#' with T the grand total, n subjects and k ratings each.  BMS and WMS agree
#' with those of stats::aov(y ~ factor(cluster)).
#'
#' @param y Ratings in long format.
#' @param cluster Subject each rating belongs to; the design must be balanced.
#' @return list: estimate (ICC(1,1)), bms, wms, sst, ssa, n, k, method.
#' @keywords internal
#' @examples
#' Icc1(c(1, 2, 3, 4, 5, 6), c(1, 1, 2, 2, 3, 3))$estimate
#' @export
Icc1 <- function(y, cluster) {
  b <- .icc_balanced(y, cluster, "icc_one_way")
  rows <- b$rows; n <- b$n; k <- b$k
  tot <- 0; tot2 <- 0; ssa <- 0
  for (i in seq_len(n)) {
    s <- 0
    for (j in seq_len(k)) {
      e <- rows[i, j]
      s <- s + e; tot <- tot + e; tot2 <- tot2 + e * e
    }
    ssa <- ssa + s * s / k
  }
  corr <- tot * tot / (n * k)
  sst <- tot2 - corr
  ssa <- ssa - corr
  bms <- ssa / (n - 1)
  wms <- (sst - ssa) / (n * (k - 1))
  den <- bms + (k - 1) * wms
  if (den == 0) stop("icc_one_way: the ratings carry no variance")
  list(estimate = (bms - wms) / den, bms = bms, wms = wms, sst = sst,
       ssa = ssa, n = n, k = k,
       method = "ICC(1) one-way random-effects model")
}

# Long-format (value, group) into a subjects-by-raters matrix.
#' Long-format (value, group) into a subjects-by-raters matrix
#'
#' A step of the icc1 implementation. Called by \code{Icc1}, \code{Icc2}, \code{Icc3}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param y Passed to \code{.s03vec}.
#' @param group Passed to \code{.s03vec}.
#' @param who See Usage.
#' @return A list with \code{rows}, \code{n}, \code{k}.
#' @export
.icc_balanced <- function(y, group, who) {
  ys <- .s03vec(y); gs <- .s03vec(group)
  if (length(ys) == 0L) stop(paste0(who, ": y is empty"))
  if (length(gs) != length(ys)) {
    stop(paste0(who, ": y and the grouping must have the same length"))
  }
  levels <- sort(unique(gs))
  cnt <- integer(length(levels))
  for (g in gs) cnt[match(g, levels)] <- cnt[match(g, levels)] + 1L
  k <- cnt[1L]
  if (any(cnt != k)) {
    stop(paste0(who, ": the design must be balanced -- every subject needs the same number of ratings"))
  }
  n <- length(levels)
  if (n < 2L) stop(paste0(who, ": need at least two subjects"))
  if (k < 2L) stop(paste0(who, ": need at least two ratings per subject"))
  rows <- matrix(0, n, k)
  fill <- integer(n)
  for (i in seq_along(ys)) {
    g <- match(gs[i], levels)
    fill[g] <- fill[g] + 1L
    rows[g, fill[g]] <- ys[i]
  }
  list(rows = rows, n = n, k = k)
}
