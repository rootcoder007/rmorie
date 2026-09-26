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
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' y <- c(2.9, 5.1, 6.8, 9.4, 11.2, 13.1, 15.0, 17.6)
#' res <- .tqang_fmod(x = x, y = y)
#' res
.tqang_fmod <- function(x, y) x - y * trunc(x / y)

#' Wrap to [-pi, pi)
#'
#' @param theta Angle in radians.
#' @return Angle in [-pi, pi).
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
#' @examples
#' d <- angular_difference(0.1, 2 * pi + 0.4)
#' abs(d - (-0.3)) < 1e-9
#' @keywords internal
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
#' @examples
#' q <- morie_tqang(c(0.1, 0.5, 1.0, -2.0), bits = 8)
#' is.list(q) || is.numeric(q)
#' @keywords internal
morie_tqang <- function(theta, bits = 4) {
  b <- as.integer(bits)
  if (!(b >= 1L && b <= 30L))
    stop("quantize_angles: bits must lie in 1..30")
  n_levels <- bitwShiftL(1L, b)
  delta <- .TWO_PI / n_levels
  th <- as.numeric(theta)
  idx <- integer(length(th))
  val <- numeric(length(th))
  err <- numeric(length(th))
  for (i in seq_along(th)) {
    w <- wrap_angle(th[i])
    k <- as.integer(floor((w + pi) / delta))
    if (k >= n_levels) k <- n_levels - 1L
    if (k < 0L) k <- 0L
    rec <- -pi + (k + 0.5) * delta
    idx[i] <- k
    val[i] <- rec
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
#' @rdname morie_tqang
#' @export
morie_quantize_angles <- morie_tqang

#' Public alias resolved by fn/_lazy_map.json
#' @rdname morie_tqang
#' @export
morie_turboquant_angle_quantization <- morie_tqang

# -- restored: morie-only definition kept through the rmorie sync --
#' Signed shortest difference \code{a - b}, in \code{[-pi, pi)}
#' @param a See Usage.
#' @param b See Usage.
#' @return Numeric scalar.
#' @export
morie_tqang_angular_difference <- function(a, b) {
  morie_tqang_wrap_angle(as.numeric(a) - as.numeric(b))
}

# -- restored: morie-only definition kept through the rmorie sync --
#' One-line rationale mirroring the Python cheatsheet
#' @return Character.
#' @export
morie_tqang_cheatsheet <- function() {
  paste0("tqang: 2^b equal sectors, delta = 2pi/2^b, codeword ",
         "-pi + (k+0.5) delta; |err| <= delta/2, MSE -> delta^2/12; ",
         "all errors use the WRAPPED difference.")
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Uniform quantisation of angles to \code{2^bits} equal sectors
#'
#' Returns a list with the sector indices, the reconstructed angles,
#' the wrapped errors, the empirical MSE, the worst-case absolute
#' error, the sector width \code{delta}, \code{half_delta}, the
#' theoretical MSE bound \code{Delta^2/12}, the \code{bits} and
#' \code{levels}.
#' @param theta Numeric vector of angles.
#' @param bits Bits per symbol (1..30).
#' @export
#' @aliases morie_tqang_tqang morie_tqang_turboquant_angle_quantization
morie_tqang_quantize_angles <- function(theta, bits = 4L) {
  b <- as.integer(bits)
  if (!(b >= 1L && b <= 30L))
    stop("quantize_angles: bits must lie in 1..30")
  n_levels <- bitwShiftL(1L, b)
  delta <- .tqang_two_pi / n_levels

  t_in <- as.numeric(theta)
  if (length(t_in) == 0L) {
    return(list(estimate = numeric(0), indices = integer(0),
                values = numeric(0), errors = numeric(0),
                mse = 0, max_abs_error = 0, delta = delta,
                half_delta = delta / 2, mse_bound = delta * delta / 12,
                bits = b, levels = n_levels,
                method = "Uniform angle quantisation on [-pi, pi), midpoint reconstruction, wrapped error"))
  }
  w <- vapply(t_in, morie_tqang_wrap_angle, numeric(1))
  k <- floor((w + pi) / delta)
  k[k >= n_levels] <- n_levels - 1L
  k[k < 0L] <- 0L
  rec <- -pi + (k + 0.5) * delta
  err <- vapply(seq_along(w), function(i)
    morie_tqang_angular_difference(w[i], rec[i]), numeric(1))
  mse <- mean(err^2)
  list(estimate = rec, indices = as.integer(k), values = rec,
       errors = err, mse = mse,
       max_abs_error = max(abs(err)),
       delta = delta, half_delta = delta / 2,
       mse_bound = delta * delta / 12, bits = b, levels = n_levels,
       method = "Uniform angle quantisation on [-pi, pi), midpoint reconstruction, wrapped error")
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Wrap an angle to \code{[-pi, pi)}
#' @param theta Numeric scalar.
#' @return Numeric scalar in \code{[-pi, pi)}.
#' @export
morie_tqang_wrap_angle <- function(theta) {
  t <- (as.numeric(theta) + pi) %% .tqang_two_pi
  if (t < 0) t <- t + .tqang_two_pi
  t - pi
}

# -- restored: morie-only objects kept through the rmorie sync --
.tqang_two_pi <- 2 * pi

morie_tqang_tqang <- morie_tqang_quantize_angles

morie_tqang_turboquant_angle_quantization <- morie_tqang_quantize_angles
