# IRT characteristic-curve linking, Haebara method.
# Source: Haebara (1980), Japanese Psychological Research 22, 144-149;
# Weeks (2010), plink, JSS 35(12), Eqs. 8, 10, 14
# (fetched-wave3/weeks-2010-plink-JSS35.pdf).  Mirrors Python
# morie.fn.linkhae (same criterion, grid, and start; optimizer is
# stats::optim Nelder-Mead, argmin agreement bound 1e-6 under the
# criterion-parity plateau doctrine).

#' .linkhae_p3pl
#'
#' A step of the linkhae_native implementation. Called by \code{morie_linkhae}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param theta Numeric; combined arithmetically in the body.
#' @param a Numeric; combined arithmetically in the body.
#' @param b Numeric; combined arithmetically in the body.
#' @param c Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.linkhae_p3pl <- function(theta, a, b, c) {
  e <- exp(a * (theta - b))
  c + (1 - c) * e / (1 + e)
}

#' Haebara characteristic-curve IRT linking
#'
#' Finds (A, B) of theta_T = A theta_F + B minimizing the summed
#' squared differences between item characteristic curves of the
#' common items (plink Eq. 14), with transformed from-scale
#' parameters a* = a_F/A, b* = A b_F + B, c* = c_F.  The symmetric
#' variant adds the reverse-direction term (a# = A a_T,
#' b# = (b_T - B)/A).
#'
#' @param items_from,items_to Matrices (or lists) of common-item 3PL
#'   parameters (a, b, c) on each scale; use c = 0 for 2PL.
#' @param symmetric Minimize Q1 + Q2 instead of Q1 only.
#' @param theta_points Optional evaluation grid (default 41 points on
#'   [-4, 4]).
#' @return A list with elements \code{A}, \code{B}, \code{criterion},
#'   \code{symmetric}, \code{n_common}, \code{method}.
#' @references Haebara, T. (1980). Equating logistic ability scales
#'   by a weighted least squares method. Japanese Psychological
#'   Research, 22, 144-149.  Weeks, J. P. (2010). JSS, 35(12).
#' @export
#' @examples
#' M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
#' morie_linkhae(M, M)
morie_linkhae <- function(items_from, items_to, symmetric = FALSE,
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
  l_norm <- length(grid) * s
  crit <- function(x) {
    A <- x[1]; B <- x[2]
    if (A <= 0) return(1e10)
    q1 <- 0
    for (th in grid) {
      p_t <- .linkhae_p3pl(th, to[, 1], to[, 2], to[, 3])
      p_star <- .linkhae_p3pl(th, fr[, 1] / A, A * fr[, 2] + B, fr[, 3])
      q1 <- q1 + sum((p_t - p_star)^2)
    }
    q <- q1 / l_norm
    if (symmetric) {
      q2 <- 0
      for (th in grid) {
        p_f <- .linkhae_p3pl(th, fr[, 1], fr[, 2], fr[, 3])
        p_hash <- .linkhae_p3pl(th, A * to[, 1], (to[, 2] - B) / A,
                                to[, 3])
        q2 <- q2 + sum((p_f - p_hash)^2)
      }
      q <- q + q2 / l_norm
    }
    q
  }
  res <- stats::optim(c(1, 0), crit, method = "Nelder-Mead",
                      control = list(reltol = 1e-14, maxit = 5000))
  list(A = res$par[1], B = res$par[2],
       criterion = res$value,
       symmetric = symmetric,
       n_common = s,
       method = "Haebara characteristic-curve linking (plink Eq. 14)")
}
