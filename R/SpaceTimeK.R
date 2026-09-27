#' Space-time K function of Diggle, Chetwynd, Haggkvist and Morris (1995)
#'
#' For events \eqn{(x_i, t_i)} in a rectangle A and interval
#' the interval from \eqn{T_0} to \eqn{T_1} (length T), with \eqn{w_{ij}} Ripley's isotropic factor
#' and \eqn{v_{ij} = 2} when the interval \eqn{t_i \pm u}, \eqn{u = |t_i -
#' t_j|}, reaches either time limit (else 1):
#' \eqn{K_s(s) = |A| / (n(n-1)) \sum w_{ij} I(d_{ij} \le s)},
#' \eqn{K_t(t) = T / (n(n-1)) \sum v_{ij} I(u_{ij} \le t)} and
#' \eqn{K_{st}(s, t) = |A| T / (n(n-1)) \sum w_{ij} v_{ij} I(d_{ij} \le s)
#' I(u_{ij} \le t)}. Under no space-time interaction \eqn{K_{st} = K_s K_t};
#' \eqn{D = K_{st} - K_s K_t} and \eqn{D_0 = D / (K_s K_t)} are returned.
#' These are the estimators of \code{splancs::stkhat}.
#'
#' @param points Event locations (n x 2 matrix) inside \code{window}.
#' @param times Event times inside \code{tlimits}.
#' @param window Rectangle \code{c(xmin, xmax, ymin, ymax)}.
#' @param tlimits \code{c(T0, T1)}.
#' @param s Spatial distances (increasing).
#' @param t Time lags (increasing).
#' @return List with \code{ks}, \code{kt}, \code{kst} (matrix, rows s),
#'   \code{D}, \code{D0}, \code{s}, \code{t}.
#' @references Diggle, P. J., Chetwynd, A. G., Haggkvist, R. and Morris,
#'   S. E. (1995). Second-order analysis of space-time clustering.
#'   Statistical Methods in Medical Research 4, 124-136.
#' @examples
#' P <- rbind(c(0.1, 0.2), c(0.3, 0.25), c(0.8, 0.9), c(0.5, 0.5))
#' SpaceTimeK(P, c(1, 1.5, 4, 2), c(0, 1, 0, 1), c(0, 5), 0.3, 1)$ks
#' @export
SpaceTimeK <- function(points, times, window, tlimits, s, t) {
  P <- as.matrix(points)
  tt <- as.numeric(times)
  w4 <- as.numeric(window)
  n <- nrow(P)
  area <- (w4[2] - w4[1]) * (w4[4] - w4[3])
  span <- tlimits[2] - tlimits[1]
  ks <- numeric(length(s))
  kt <- numeric(length(t))
  kst <- matrix(0, length(s), length(t))
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i == j) next
      d <- sqrt(sum((P[i, ] - P[j, ])^2))
      u <- abs(tt[i] - tt[j])
      w <- 1 / .ripk_weight(P[i, 1], P[i, 2], d, w4[1], w4[2], w4[3], w4[4])
      v <- if (tt[i] - tlimits[1] <= u || tlimits[2] - tt[i] <= u) 2 else 1
      ins <- d <= s
      int <- u <= t
      ks <- ks + w * ins
      kt <- kt + v * int
      kst <- kst + w * v * outer(ins, int)
    }
  }
  cc <- n * (n - 1)
  ks <- area * ks / cc
  kt <- span * kt / cc
  kst <- area * span * kst / cc
  D <- kst - outer(ks, kt)
  list(ks = ks, kt = kt, kst = kst, D = D, D0 = D / outer(ks, kt), s = s, t = t)
}
