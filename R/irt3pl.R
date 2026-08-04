# R arm of morie/fn/irt3pl.py -- three-parameter logistic IRT model.
#
# The Python body was a placeholder: it averaged a leading `y` argument and
# never referenced theta, a, b or c. There was no R arm at all.
#
#   P_i(theta) = c_i + (1 - c_i) / (1 + exp(-a_i (theta - b_i)))
#
# Birnbaum (1968), in Lord & Novick, Statistical Theories of Mental Test
# Scores, chs. 17-20; Lord (1980). The 2PL kernel is Samejima (1969),
# Psychometric Monograph 17, eq. (10-13) p. 79.
#
# Helpers live in aaa_helpers_irt.R.

#' @noRd
morie_three_parameter_logistic <- function(theta, a = 1, b = 0, c = 0) {
  th <- as.numeric(theta)
  n <- length(th)
  if (n == 0L) stop("theta is empty.", call. = FALSE)
  av <- .irt_broadcast(a, n, "a")
  bv <- .irt_broadcast(b, n, "b")
  cv <- .irt_broadcast(c, n, "c")

  if (any(is.na(cv)) || any(cv < 0) || any(cv >= 1)) {
    stop("c must lie in [0, 1).", call. = FALSE)
  }
  if (any(!is.finite(av))) stop("a must be finite.", call. = FALSE)

  logit <- av * (th - bv)
  p <- cv + (1 - cv) * .irt_expit(logit)

  list(p = p, logit = logit, theta = th, a = av, b = bv, c = cv, n = n,
       method = "Three-parameter logistic IRT model (Birnbaum 1968)")
}

#' @noRd
Irt3pl <- morie_three_parameter_logistic
