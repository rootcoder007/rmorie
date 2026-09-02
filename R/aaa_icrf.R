# R arm of morie/fn/icrf.py -- item characteristic curve, 3PL.
#
# The Python body was a placeholder: it took the mean and standard error
# of a leading `y` argument and never referenced theta, a, b or c, so it
# returned the same number whatever item parameters it was given. There
# was no R arm at all. Birnbaum (1968), in Lord & Novick, Statistical
# Theories of Mental Test Scores, chs. 17-20, eq. (17.4.5):
#
#     P_i(theta) = c_i + (1 - c_i) / (1 + exp(-a_i (theta - b_i)))

#' @noRd
.icrf_expit <- function(z) {
  # branch so neither tail overflows; identical to the Python arm
  out <- numeric(length(z))
  pos <- z >= 0
  out[pos] <- 1 / (1 + exp(-z[pos]))
  e <- exp(z[!pos])
  out[!pos] <- e / (1 + e)
  out
}

#' @noRd
.icrf_broadcast <- function(v, n, name) {
  v <- as.numeric(v)
  if (length(v) == 1L) {
    return(rep(v, n))
  }
  if (length(v) != n) {
    stop(sprintf(
      "%s has length %d; expected 1 or %d to match theta",
      name, length(v), n
    ), call. = FALSE)
  }
  v
}

#' @noRd
morie_item_characteristic_curve <- function(theta, a = 1, b = 0, c = 0) {
  th <- as.numeric(theta)
  n <- length(th)
  if (n == 0L) stop("theta is empty.", call. = FALSE)
  av <- .icrf_broadcast(a, n, "a")
  bv <- .icrf_broadcast(b, n, "b")
  cv <- .icrf_broadcast(c, n, "c")

  if (any(is.na(cv)) || any(cv < 0) || any(cv >= 1)) {
    stop("c must lie in [0, 1).", call. = FALSE)
  }
  if (any(!is.finite(av))) {
    stop("a must be finite.", call. = FALSE)
  }

  logit <- av * (th - bv)
  p <- cv + (1 - cv) * .icrf_expit(logit)

  list(
    p = p, logit = logit, theta = th, a = av, b = bv, c = cv, n = n,
    method = "Item characteristic curve, 3PL (Birnbaum 1968)"
  )
}

#' @noRd
Icrf <- morie_item_characteristic_curve
