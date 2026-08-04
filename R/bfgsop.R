# SPDX-License-Identifier: AGPL-3.0-or-later
#' BFGS secant update of the Hessian and its inverse.
#'
#' B <- B - B s s'B/(s'B s) + y y'/(y's), or in inverse form
#' H <- (I - rho s y') H (I - rho y s') + rho s s' with rho = 1/(y's).
#'
#' @param H Current approximation; the inverse Hessian when inverse is
#'   TRUE, the Hessian otherwise.
#' @param s Step x_{k+1} - x_k.
#' @param y Gradient change g_{k+1} - g_k.
#' @param inverse Update the inverse Hessian.
#'
#' @return List with M, rho, curvature, secant, p, inverse.  secant is
#'   the largest violation of the secant condition, which is zero up to
#'   rounding for a correct update.
#' @references Broyden (1970), Fletcher (1970), Goldfarb (1970), Shanno
#'   (1970); stated identically in Nocedal and Wright, Numerical
#'   Optimization, 2nd edn, Eqs. (6.17) and (6.19).  Standard published
#'   form; none of the 1970 papers is in the local corpus and none was
#'   read.
#' @export
Bfgsupd <- function(H, s, y, inverse = TRUE) {
  M <- .t1_mat(H); p <- nrow(M)
  if (ncol(M) != p) stop("H must be square")
  s <- .t1_vec(s); y <- .t1_vec(y)
  if (length(s) != p || length(y) != p)
    stop("s and y must match the dimension of H")
  ys <- sum(y * s)
  if (ys <= 0) stop("curvature condition y's > 0 is violated")
  rho <- 1 / ys
  if (isTRUE(inverse)) {
    L <- diag(p) - rho * outer(s, y)
    R <- diag(p) - rho * outer(y, s)
    N <- L %*% M %*% R + rho * outer(s, s)
    dim(N) <- c(p, p)
    gap <- max(abs(as.numeric(N %*% y) - s))
  } else {
    Bs <- as.numeric(M %*% s)
    sBs <- sum(s * Bs)
    if (sBs <= 0) stop("s'Bs must be strictly positive")
    N <- M - outer(Bs, Bs) / sBs + outer(y, y) / ys
    dim(N) <- c(p, p)
    gap <- max(abs(as.numeric(N %*% s) - y))
  }
  .t1_result(M = N, rho = rho, curvature = ys, secant = gap, p = p,
             inverse = isTRUE(inverse),
             method = "BFGS rank-two secant update (Broyden-Fletcher-Goldfarb-Shanno 1970)")
}
