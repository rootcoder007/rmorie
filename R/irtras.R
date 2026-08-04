# R arm of morie/fn/irtras.py -- rating scale model over items.
#
# The Python body was a placeholder: it averaged a leading `X` argument and
# used `ncats` for nothing. There was no R arm at all.
#
# Each item i has its own location b_i but all items share one set of m
# thresholds tau_1..tau_m -- the shared threshold structure is exactly what
# separates the rating scale model from the partial credit model:
#
#   P(X_vi = h) prop. exp{ h (theta_v - b_i) - sum_{j<=h} tau_j }
#
# Andrich (1978) Psychometrika 43(4):561-573, doi:10.1007/BF02293814;
# Mair & Hatzinger (2007) JSS 20(9) eq. (5) p. 4.
#
# Item information is exactly the variance of the category score, since the
# linear predictor is h*theta plus terms free of theta; test information is
# the sum over items by local independence.
#
# Helpers live in aaa_helpers_irt.R.

#' @noRd
morie_rating_scale_model <- function(theta, b, tau) {
  th <- as.numeric(theta)
  n <- length(th)
  if (n == 0L) stop("theta is empty.", call. = FALSE)
  bv <- as.numeric(b)
  k <- length(bv)
  if (k == 0L) stop("b is empty.", call. = FALSE)
  tv <- as.numeric(tau)
  if (length(tv) == 0L) {
    stop("tau is empty; a rating scale needs at least one threshold.",
         call. = FALSE)
  }
  scores <- seq_len(length(tv) + 1L) - 1L

  p <- vector("list", k)
  expected <- matrix(0, n, k)
  info <- matrix(0, n, k)
  for (i in seq_len(k)) {
    pi <- matrix(0, n, length(tv) + 1L)
    for (v in seq_len(n)) {
      r <- .irt_rsm_probs(th[v], bv[i], tv)
      m <- .irt_cat_moments(r$p, scores)
      pi[v, ] <- r$p
      expected[v, i] <- m[["mean"]]
      info[v, i] <- m[["var"]]
    }
    p[[i]] <- pi
  }

  list(p = p, expected = expected, info = info,
       test_expected = as.numeric(rowSums(expected)),
       test_info = as.numeric(rowSums(info)),
       theta = th, b = bv, tau = tv, ncat = length(tv) + 1L, n = n, k = k,
       method = "Rating scale model, shared thresholds (Andrich 1978)")
}

#' @noRd
Irtras <- morie_rating_scale_model
