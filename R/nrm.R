# R arm of morie/fn/nrm.py -- Bock nominal response model, one item.
#
# The Python body was a placeholder: it averaged a leading `y` argument and
# never referenced theta, a_k or c_k. There was no R arm at all.
#
#   P(X = r | theta) = exp(a_r theta + c_r) / sum_s exp(a_s theta + c_s)
#
# Bock (1972) Psychometrika 37(1):29-51, doi:10.1007/BF02291411. Printed as
# eq. (14) p. 16 of Tutz (2020), arXiv:2010.01382, in the form
# exp(alpha_ir theta_p - beta_ir); c_r = -beta_ir here.
#
# Helpers live in aaa_helpers_irt.R.

#' @noRd
morie_nominal_response_bock <- function(theta, a_k = c(0, 1), c_k = c(0, 0)) {
  th <- as.numeric(theta)
  n <- length(th)
  if (n == 0L) stop("theta is empty.", call. = FALSE)
  av <- as.numeric(a_k)
  cv <- as.numeric(c_k)
  if (length(av) < 2L) stop("a_k needs at least two categories.", call. = FALSE)
  if (length(cv) != length(av)) {
    stop(sprintf("c_k has length %d; expected %d to match a_k",
                 length(cv), length(av)), call. = FALSE)
  }

  p <- matrix(0, n, length(av))
  eta <- matrix(0, n, length(av))
  expected <- numeric(n)
  info <- numeric(n)
  for (v in seq_len(n)) {
    r <- .irt_nrm_probs(th[v], av, cv)
    m <- .irt_cat_moments(r$p, av)
    p[v, ] <- r$p
    eta[v, ] <- r$eta
    expected[v] <- m[["mean"]]
    info[v] <- m[["var"]]
  }

  list(p = p, eta = eta, expected = expected, info = info, theta = th,
       a_k = av, c_k = cv, ncat = length(av), n = n,
       method = "Bock nominal response model, one item (Bock 1972)")
}

#' @noRd
Nrm <- morie_nominal_response_bock
