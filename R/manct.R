# SPDX-License-Identifier: AGPL-3.0-or-later
#' Continuity correction for a 2x2 meta-analytic table
#'
#' Sweeting, Sutton and Lambert (2004), What to add to nothing?  Use and
#' avoidance of continuity corrections in meta-analysis of sparse data,
#' Statistics in Medicine 23(9), 1351-1375, compare the constant, the
#' treatment-arm and the empirical schemes.  The paper is paywalled; the
#' constant scheme implemented here -- add c to every cell -- is stated
#' identically wherever the correction is defined.  By default it is
#' applied only when a cell is zero, which is the standard practice
#' Sweeting et al. describe.
#'
#' @param a,b events and non-events in the treatment arm.
#' @param c,d events and non-events in the control arm.
#' @param cc the constant to add; 0.5 is the usual choice.
#' @param always add the correction even when no cell is zero.
#' @return list: a_adj, b_adj, c_adj, d_adj, estimate (log OR), se,
#'   applied, cc, method.
#' @keywords internal
#' @examples
#' Contcorr(0, 20, 5, 15)$estimate
#' @export
Contcorr <- function(a, b, c, d, cc = 0.5, always = FALSE) {
  a0 <- as.numeric(a)
  b0 <- as.numeric(b)
  c0 <- as.numeric(c)
  d0 <- as.numeric(d)
  zero <- (a0 == 0) || (b0 == 0) || (c0 == 0) || (d0 == 0)
  add <- if (always || zero) as.numeric(cc) else 0
  aa <- a0 + add
  bb <- b0 + add
  ccc <- c0 + add
  dd <- d0 + add
  if (aa > 0 && bb > 0 && ccc > 0 && dd > 0) {
    lor <- log((aa * dd) / (bb * ccc))
    se <- sqrt(1 / aa + 1 / bb + 1 / ccc + 1 / dd)
  } else {
    lor <- NaN
    se <- NaN
  }
  list(a_adj = aa, b_adj = bb, c_adj = ccc, d_adj = dd,
       estimate = lor, se = se, applied = add > 0, cc = add,
       method = "Constant continuity correction for sparse 2x2 tables")
}
