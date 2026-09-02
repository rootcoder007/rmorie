# Uniform angle quantisation on [-pi, pi).
# Sources: the standard uniform-scalar-quantiser result on a flat
# density, with the wraparound difference (a - b + pi) mod 2pi - pi so
# an angle just below pi and one just above -pi are reported as
# neighbours, not as opposites.
#
# Native implementation mirroring Python morie.fn.tqang exactly: the
# same floor-on-the-shifted-wrapped-angle assignment, the same
# boundary clamp, the same midpoint reconstruction, the same
# MSE = delta^2/12 bound, the same wrapped error.

.TWO_PI <- 2 * pi

#' Wrap to [-pi, pi)
#'
#' @param x See Usage.
#' @param y See Usage.
#' @return Angle in [-pi, pi).
#' @export
.tqang_fmod <- function(x, y) x - y * trunc(x / y)

wrap_angle <- function(theta) {
  t <- .tqang_fmod(as.numeric(theta) + pi, .TWO_PI)
  if (t < 0) t <- t + .TWO_PI
  t - pi
}

#' Signed shortest difference a - b, in [-pi, pi)
#'
#' @param a First angle.
#' @param b Second angle.
#' @return Wrapped difference in [-pi, pi).
#' @export
angular_difference <- function(a, b) wrap_angle(as.numeric(a) - as.numeric(b))

#' Quantise angles to 2^bits uniform sectors
#'
#' @param theta Vector of angles in radians.
#' @param bits Bits per code.
#' @return A list with \code{estimate}, \code{indices}, \code{values},
#'   \code{errors}, \code{mse}, \code{max_abs_error}, \code{delta},
#'   \code{half_delta}, \code{mse_bound}, \code{bits}, \code{levels},
#'   \code{method}.
#' @export
morie_tqang <- function(theta, bits = 4) {
  b <- as.integer(bits)
  if (!(b >= 1L && b <= 30L))
    stop("quantize_angles: bits must lie in 1..30")
  n_levels <- bitwShiftL(1L, b)
  delta <- .TWO_PI / n_levels
  th <- as.numeric(theta)
  idx <- integer(length(th)); val <- numeric(length(th))
  err <- numeric(length(th))
  for (i in seq_along(th)) {
    w <- wrap_angle(th[i])
    k <- as.integer(floor((w + pi) / delta))
    if (k >= n_levels) k <- n_levels - 1L
    if (k < 0L) k <- 0L
    rec <- -pi + (k + 0.5) * delta
    idx[i] <- k; val[i] <- rec
    err[i] <- angular_difference(w, rec)
  }
  mse <- if (length(err) > 0L) mean(err^2) else 0
  list(estimate = val, indices = idx, values = val, errors = err,
       mse = mse, max_abs_error = max(abs(err)),
       delta = delta, half_delta = 0.5 * delta,
       mse_bound = delta^2 / 12, bits = b, levels = n_levels,
       method = "Uniform angle quantisation on [-pi, pi), midpoint reconstruction, wrapped error")
}

#' Public alias resolved by fn/_lazy_map.json
#' @export
#' @noRd
morie_quantize_angles <- morie_tqang

#' Public alias resolved by fn/_lazy_map.json
#' @export
#' @noRd
morie_turboquant_angle_quantization <- morie_tqang
