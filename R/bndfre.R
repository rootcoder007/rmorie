# SPDX-License-Identifier: AGPL-3.0-or-later
#' Imbens-Manski interval from replicated estimates of the two bounds
#'
#' A thin front end over \code{morie_bnd_imbens_manski}, which is where the
#' Imbens-Manski critical value actually lives; this function only reduces
#' two samples of bound estimates to the arguments that construction needs.
#' Aliasing rather than re-deriving matters here: a second copy of the
#' critical value would agree with the first at 1e-9 forever and still be a
#' second copy.
#'
#' Formula: \code{[l - c s_l / sqrt(n), u + c s_u / sqrt(n)]} with \code{c}
#' solving \code{Phi(c + sqrt(n) Delta / max(s_l, s_u)) - Phi(-c) = 1 - alpha}.
#'
#' @param lower,upper Replicated estimates of the two bounds, same length.
#' @param alpha Miss probability, default 0.05.
#' @return List with \code{lower}, \code{upper}, \code{width}, \code{c},
#'   \code{z_one_sided}, \code{z_two_sided}, \code{delta}, \code{n}.
#' @references Imbens, G. W. and Manski, C. F. (2004). Confidence intervals
#'   for partially identified parameters. Econometrica 72(6), 1845-1857,
#'   equation (6). \doi{10.1111/j.1468-0262.2004.00555.x}.
#' @export
Bndfre <- function(lower, upper, alpha = 0.05) {
  lo <- as.numeric(unlist(lower))
  hi <- as.numeric(unlist(upper))
  n <- length(lo)
  if (n < 2L) stop("Bndfre: need at least two replicates")
  if (length(hi) != n)
    stop("Bndfre: lower and upper must have the same length")
  tl <- mean(lo)
  tu <- mean(hi)
  if (tu < tl) stop("Bndfre: mean upper is below mean lower")
  sl <- stats::sd(lo)
  su <- stats::sd(hi)
  if (sl <= 0) sl <- 1e-12
  if (su <= 0) su <- 1e-12
  r <- morie_bnd_imbens_manski(tl, tu, sl, su, n, alpha)
  .t1_result(lower = r$ci[1], upper = r$ci[2],
             width = r$ci[2] - r$ci[1], c = r$c,
             z_one_sided = r$z_one_sided, z_two_sided = r$z_two_sided,
             delta = r$delta, n = n,
             method = "Frequentist bound with valid coverage")
}
