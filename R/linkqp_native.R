# IRT characteristic-curve linking, Stocking-Lord method.
# Source: Stocking & Lord (1983), Applied Psychological Measurement
# 7, 201-210; Weeks (2010), plink, JSS 35(12), Eqs. 8, 10, 15
# (fetched-wave3/weeks-2010-plink-JSS35.pdf).  Mirrors Python
# morie.fn.linkqp (same criterion, grid, and start; optimizer is
# stats::optim Nelder-Mead, argmin agreement bound 1e-6 under the
# criterion-parity plateau doctrine).

#' .linkqp_p3pl
#'
#' A step of the linkqp_native implementation. Called by \code{morie_linkqp}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param theta Numeric; combined arithmetically in the body.
#' @param a Numeric; combined arithmetically in the body.
#' @param b Numeric; combined arithmetically in the body.
#' @param c Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.linkqp_p3pl <- function(theta, a, b, c) {
  e <- exp(a * (theta - b))
  c + (1 - c) * e / (1 + e)
}

#' Stocking-Lord characteristic-curve IRT linking
#'
#' Finds (A, B) of theta_T = A theta_F + B minimizing squared
#' differences between test characteristic curves of the common items
#' (plink Eq. 15), with transformed from-scale parameters a* = a_F/A,
#' b* = A b_F + B, c* = c_F.  The symmetric variant adds the
#' reverse-direction term.
#'
#' @param items_from,items_to Matrices (or lists) of common-item 3PL
#'   parameters (a, b, c) on each scale; use c = 0 for 2PL.
#' @param symmetric Minimize F1 + F2 instead of F1 only.
#' @param theta_points Optional evaluation grid (default 41 points on
#'   \[-4, 4\]).
#' @return A list with elements \code{A}, \code{B}, \code{criterion},
#'   \code{symmetric}, \code{n_common}, \code{method}.
#' @references Stocking, M. L. and Lord, F. M. (1983). Developing a
#'   common metric in item response theory. Applied Psychological
#'   Measurement, 7, 201-210.  Weeks, J. P. (2010). JSS, 35(12).
#' @export
#' @examples
#' M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
#' morie_linkqp(M, M)
morie_linkqp <- function(items_from, items_to, symmetric = FALSE,
                         theta_points = NULL) {
  fr <- if (is.matrix(items_from)) items_from else
    do.call(rbind, lapply(items_from, as.numeric))
  to <- if (is.matrix(items_to)) items_to else
    do.call(rbind, lapply(items_to, as.numeric))
  s <- nrow(fr)
  if (nrow(to) != s || s < 2) {
    stop("need >= 2 common items with matching lengths")
  }
  grid <- if (is.null(theta_points)) seq(-4, 4, length.out = 41) else
    as.numeric(theta_points)
  l_norm <- length(grid)
  crit <- function(x) {
    A <- x[1]; B <- x[2]
    if (A <= 0) return(1e10)
    f1 <- 0
    for (th in grid) {
      tcc_t <- sum(.linkqp_p3pl(th, to[, 1], to[, 2], to[, 3]))
      tcc_star <- sum(.linkqp_p3pl(th, fr[, 1] / A, A * fr[, 2] + B,
                                   fr[, 3]))
      f1 <- f1 + (tcc_t - tcc_star)^2
    }
    f <- f1 / l_norm
    if (symmetric) {
      f2 <- 0
      for (th in grid) {
        tcc_f <- sum(.linkqp_p3pl(th, fr[, 1], fr[, 2], fr[, 3]))
        tcc_hash <- sum(.linkqp_p3pl(th, A * to[, 1],
                                     (to[, 2] - B) / A, to[, 3]))
        f2 <- f2 + (tcc_f - tcc_hash)^2
      }
      f <- f + f2 / l_norm
    }
    f
  }
  res <- stats::optim(c(1, 0), crit, method = "Nelder-Mead",
                      control = list(reltol = 1e-14, maxit = 5000))
  list(A = res$par[1], B = res$par[2],
       criterion = res$value,
       symmetric = symmetric,
       n_common = s,
       method = "Stocking-Lord characteristic-curve linking (plink Eq. 15)")
}
