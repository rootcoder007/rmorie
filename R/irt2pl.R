# R arm of morie/fn/irt2pl.py -- two-parameter logistic IRT model.
#
# The Python body was a placeholder: it averaged a leading `y` argument and
# never touched theta, a or b, so every item returned the same number. There
# was no R arm at all.
#
#   P_i(theta) = 1 / (1 + exp(-a_i (theta - b_i)))
#
# Birnbaum (1968), in Lord & Novick, Statistical Theories of Mental Test
# Scores, chs. 17-20. Samejima (1969), Psychometric Monograph 17, eq. (10-13)
# p. 79 states the logistic model for dichotomous items in the D-scaled form
# 1/(1 + exp(-D a (theta - b))); the logistic metric used here is D = 1.
#
# Helpers live in aaa_helpers_irt.R.

#' @noRd
morie_two_parameter_logistic <- function(theta, a = 1, b = 0) {
  th <- as.numeric(theta)
  n <- length(th)
  if (n == 0L) stop("theta is empty.", call. = FALSE)
  av <- .irt_broadcast(a, n, "a")
  bv <- .irt_broadcast(b, n, "b")
  if (any(!is.finite(av))) stop("a must be finite.", call. = FALSE)

  logit <- av * (th - bv)
  p <- .irt_expit(logit)

  list(p = p, logit = logit, theta = th, a = av, b = bv, n = n,
       method = "Two-parameter logistic IRT model (Birnbaum 1968)")
}

#' @noRd
Irt2pl <- morie_two_parameter_logistic
