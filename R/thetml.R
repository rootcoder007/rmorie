# R arm of morie/fn/thetml.py -- maximum likelihood estimate of theta.
#
# The Python body was a placeholder: it averaged a leading `X` argument, the
# second argument `items` was never used, and no item parameter entered the
# result. There was no R arm at all.
#
#   l(theta)  = sum_j x_j log P_j + (1 - x_j) log(1 - P_j)
#   l'(theta) = sum_j (x_j - P_j) P'_j / (P_j Q_j)
#   I(theta)  = sum_j (P'_j)^2 / (P_j Q_j)          [Samejima 1969 eq. (6-9)]
#
# with P_j the 3PL curve and P'_j = a_j (1 - c_j) P*_j Q*_j.
#
# The maximiser is found deterministically: a fixed 1201-point grid on
# [-6, 6] picks the global mode (the 3PL likelihood can be multimodal), then
# 60 Fisher-scoring steps with the step clamped to +/- 0.5. Both arms visit
# the identical sequence of iterates.
#
# Helpers live in aaa_helpers_irt.R.

#' @noRd
morie_theta_mle <- function(x, a = 1, b = 0, c = 0) {
  xs <- as.numeric(x)
  n <- length(xs)
  if (n == 0L) stop("x is empty.", call. = FALSE)
  if (any(is.na(xs)) || any(!(xs %in% c(0, 1)))) {
    stop("x must contain only 0 and 1.", call. = FALSE)
  }
  av <- .irt_broadcast(a, n, "a")
  bv <- .irt_broadcast(b, n, "b")
  cv <- .irt_broadcast(c, n, "c")
  if (any(is.na(cv)) || any(cv < 0) || any(cv >= 1)) {
    stop("c must lie in [0, 1).", call. = FALSE)
  }
  if (any(!is.finite(av))) stop("a must be finite.", call. = FALSE)

  LOWER <- -6
  UPPER <- 6
  NGRID <- 1201L
  MAXIT <- 60L

  parts <- function(t) {
    ps <- .irt_expit(av * (t - bv))
    p <- cv + (1 - cv) * ps
    q <- 1 - p
    d <- av * (1 - cv) * ps * (1 - ps)
    c(ll = sum(xs * log(p) + (1 - xs) * log(q)),
      sc = sum((xs - p) * d / (p * q)),
      fi = sum(d * d / (p * q)))
  }

  r <- sum(xs)

  if (r == 0 || r == n) {
    t0 <- if (r == n) UPPER else LOWER
    ll0 <- if (all(cv == 0)) 0 else unname(parts(t0)["ll"])
    return(list(theta = if (r == n) Inf else -Inf,
                se = Inf, loglik = ll0, score = NA_real_, information = 0,
                raw_score = r, n_items = n, converged = FALSE,
                method = "MLE of theta, 3PL (Birnbaum 1968)"))
  }

  step <- (UPPER - LOWER) / (NGRID - 1L)
  best <- LOWER
  bestll <- unname(parts(LOWER)["ll"])
  for (k in seq_len(NGRID - 1L)) {
    tk <- LOWER + k * step
    ll <- unname(parts(tk)["ll"])
    if (ll > bestll) {
      bestll <- ll
      best <- tk
    }
  }

  t <- best
  for (it in seq_len(MAXIT)) {
    pr <- parts(t)
    if (pr[["fi"]] <= 0) break
    d <- pr[["sc"]] / pr[["fi"]]
    if (d > 0.5) d <- 0.5 else if (d < -0.5) d <- -0.5
    t <- t + d
  }

  pr <- parts(t)
  list(theta = t,
       se = if (pr[["fi"]] > 0) 1 / sqrt(pr[["fi"]]) else Inf,
       loglik = unname(pr[["ll"]]), score = unname(pr[["sc"]]),
       information = unname(pr[["fi"]]),
       raw_score = r, n_items = n, converged = abs(pr[["sc"]]) < 1e-8,
       method = "MLE of theta, 3PL (Birnbaum 1968)")
}

#' @noRd
Thetml <- morie_theta_mle
