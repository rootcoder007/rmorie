# R arm of morie/fn/rsmand.py -- Andrich rating scale model, one item.
#
# The Python body was a placeholder: it averaged a leading `y` argument and
# never referenced theta, b or tau_j. There was no R arm at all.
#
#   P(X = h | theta) prop. exp{ sum_{j=1}^{h} (theta - b - tau_j) }
#                        = exp{ h (theta - b) - sum_{j<=h} tau_j },  h = 0..m
#
# Andrich (1978) Psychometrika 43(4):561-573, doi:10.1007/BF02293814.
# Printed as eq. (5) p. 4 of Mair & Hatzinger (2007) JSS 20(9) in the
# easiness parameterisation exp[h(theta_v + beta_i) + omega_h]; that is this
# formula with beta_i = -b and omega_h = -sum_{j<=h} tau_j.
#
# Helpers live in aaa_helpers_irt.R.

#' @noRd
morie_rating_scale_andrich <- function(theta, b = 0, tau = 0) {
  th <- as.numeric(theta)
  n <- length(th)
  if (n == 0L) stop("theta is empty.", call. = FALSE)
  tv <- as.numeric(tau)
  if (length(tv) == 0L) {
    stop("tau is empty; a rating scale needs at least one threshold.",
         call. = FALSE)
  }
  bb <- as.numeric(b)[1L]
  scores <- seq_len(length(tv) + 1L) - 1L

  p <- matrix(0, n, length(tv) + 1L)
  eta <- matrix(0, n, length(tv) + 1L)
  expected <- numeric(n)
  info <- numeric(n)
  for (v in seq_len(n)) {
    r <- .irt_rsm_probs(th[v], bb, tv)
    m <- .irt_cat_moments(r$p, scores)
    p[v, ] <- r$p
    eta[v, ] <- r$eta
    expected[v] <- m[["mean"]]
    info[v] <- m[["var"]]
  }

  list(p = p, eta = eta, expected = expected, info = info, theta = th,
       b = bb, tau = tv, ncat = length(tv) + 1L, n = n,
       method = "Andrich rating scale model, one item (Andrich 1978)")
}

#' @noRd
Rsmand <- morie_rating_scale_andrich
