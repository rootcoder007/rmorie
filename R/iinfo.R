# R arm of morie/fn/iinfo.py -- the item information function.
#
# The Python body was a placeholder: it averaged a leading `y` argument and
# ignored every item parameter. There was no R arm at all.
#
# Samejima (1969), Psychometric Monograph 17, eq. (6-9) p. 39:
#
#   I_g(theta) = {P'_g(theta)}^2 / (P_g(theta) Q_g(theta))
#
# named on p. 40 "the item information function ... by Birnbaum". For the
# 3PL curve P = c + (1 - c) P*, P* = 1/(1 + exp(-a(theta - b))), we have
# P' = a (1 - c) P* Q*, so
#
#   I(theta) = a^2 (1 - c)^2 (P* Q*)^2 / (P Q),
#
# which collapses to a^2 P Q when c = 0.
#
# Helpers live in aaa_helpers_irt.R.

#' @noRd
morie_item_information <- function(theta, a = 1, b = 0, c = 0) {
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

  ps <- .irt_expit(av * (th - bv))
  qs <- 1 - ps
  p <- cv + (1 - cv) * ps
  q <- 1 - p
  dp <- av * (1 - cv) * ps * qs
  info <- dp * dp / (p * q)

  list(info = info, p = p, dp = dp, theta = th, a = av, b = bv, c = cv,
       n = n, total = sum(info),
       method = "Item information function (Birnbaum; Samejima 1969 eq. 6-9)")
}

#' @noRd
Iinfo <- morie_item_information
