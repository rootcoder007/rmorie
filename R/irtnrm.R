# R arm of morie/fn/irtnrm.py -- nominal response model over items.
#
# The Python body was a placeholder: it averaged a leading `X` argument and
# used `ncats` for nothing. There was no R arm at all.
#
#   P(X_vi = r) = exp(a_ir theta_v + c_ir) / sum_s exp(a_is theta_v + c_is)
#
# The category-specific slopes a_ir are what make this nominal rather than
# ordinal: nothing constrains them to be ordered.
#
# Bock (1972) Psychometrika 37(1):29-51, doi:10.1007/BF02291411; printed as
# eq. (14) p. 16 of Tutz (2020), arXiv:2010.01382.
#
# Helpers live in aaa_helpers_irt.R.

#' @noRd
morie_nominal_response <- function(theta, a, c) {
  th <- as.numeric(theta)
  n <- length(th)
  if (n == 0L) stop("theta is empty.", call. = FALSE)
  am <- .irt_as_matrix(a, "a")
  cm <- .irt_as_matrix(c, "c")
  k <- nrow(am)
  if (nrow(cm) != k || ncol(cm) != ncol(am)) {
    stop("c must have the same shape as a.", call. = FALSE)
  }
  ncat <- ncol(am)
  if (ncat < 2L) stop("a needs at least two categories.", call. = FALSE)

  p <- vector("list", k)
  expected <- matrix(0, n, k)
  info <- matrix(0, n, k)
  for (i in seq_len(k)) {
    pi <- matrix(0, n, ncat)
    for (v in seq_len(n)) {
      r <- .irt_nrm_probs(th[v], am[i, ], cm[i, ])
      m <- .irt_cat_moments(r$p, am[i, ])
      pi[v, ] <- r$p
      expected[v, i] <- m[["mean"]]
      info[v, i] <- m[["var"]]
    }
    p[[i]] <- pi
  }

  list(p = p, expected = expected, info = info,
       test_info = as.numeric(rowSums(info)),
       theta = th, a = am, c = cm, ncat = ncat, n = n, k = k,
       method = "Nominal response model, category-specific slopes (Bock 1972)")
}

#' @noRd
Irtnrm <- morie_nominal_response
