# Ghosal & van der Vaart (2017) shelf: R mirror of the morie Python
# core formulas (src/morie/fn/_bnp_core.py + gh_* modules).
# Certified equations: (3.2)-(3.3), (3.23), (4.10)-(4.12), Prop 4.8,
# Prop 4.20, Thm 4.25, Prop 4.10 (Ewens), PY EPPF (sec 14.4),
# eq (8.1), App B/G/J.

#' @noRd
morie_gh_stick_breaking <- function(V) {
  stopifnot(all(V >= 0 & V <= 1))
  out <- numeric(length(V))
  left <- 1
  for (j in seq_along(V)) {
    out[j] <- left * V[j]
    left <- left * (1 - V[j])
  }
  out
}

#' @noRd
morie_gh_discrete_hazard <- function(p) {
  out <- numeric(length(p))
  cum <- 0
  for (j in seq_along(p)) {
    denom <- 1 - cum
    out[j] <- if (denom > 1e-300) p[j] / denom else 1
    cum <- cum + p[j]
  }
  out
}

#' @noRd
morie_gh_dp_posterior <- function(G0_A, alpha, n_in_A, n) {
  pn <- if (n > 0) n_in_A / n else 0
  m <- alpha / (alpha + n) * G0_A + n / (alpha + n) * pn
  v <- m * (1 - m) / (1 + alpha + n)
  list(mean = m, var = v, precision = alpha + n)
}

#' @noRd
morie_gh_dp_ndistinct <- function(n, alpha) {
  i <- seq_len(n)
  list(mean = sum(alpha / (alpha + i - 1)),
       var = sum(alpha * (i - 1) / (alpha + i - 1)^2))
}

#' @noRd
morie_gh_eps_dp_size <- function(eps, alpha) {
  2 + alpha * (-log(eps))
}

#' @noRd
morie_gh_dp_median_cdf <- function(G_x, alpha, n_grid = 4000L) {
  a <- alpha * G_x
  b <- alpha * (1 - G_x)
  h <- 0.5 / n_grid
  u <- 0.5 + (seq_len(n_grid) - 0.5) * h
  u <- u[u < 1]
  sum(exp(lgamma(a + b) - lgamma(a) - lgamma(b) +
            (a - 1) * log(u) + (b - 1) * log(1 - u))) * h
}

#' @noRd
morie_gh_ewens_log <- function(multiplicities, alpha) {
  m <- as.numeric(multiplicities)
  n <- sum(seq_along(m) * m)
  lp <- lgamma(n + 1) - sum(log(alpha + seq_len(n) - 1))
  for (i in seq_along(m)) {
    lp <- lp + m[i] * log(alpha) - m[i] * log(i) - lgamma(m[i] + 1)
  }
  lp
}

#' @noRd
morie_gh_py_eppf_log <- function(sizes, d, theta) {
  n <- sum(sizes)
  k <- length(sizes)
  lp <- 0
  if (k > 1) for (j in seq_len(k - 1)) lp <- lp + log(theta + j * d)
  for (i in seq_len(n - 1)) lp <- lp - log(theta + i)
  for (nj in sizes) {
    if (nj > 1) for (l in 0:(nj - 2)) lp <- lp + log(1 - d + l)
  }
  lp
}

#' @noRd
morie_gh_wn_posterior <- function(X, n, prior_var) {
  lam <- prior_var
  list(mean = n * X / (n + 1 / lam), var = 1 / (n + 1 / lam))
}

#' @noRd
morie_gh_pt_bits <- function(x, depth) {
  out <- integer(depth)
  v <- min(max(x, 0), 1 - 1e-15)
  for (m in seq_len(depth)) {
    v <- v * 2
    out[m] <- as.integer(v >= 1)
    v <- v - out[m]
  }
  out
}

#' @noRd
morie_gh_pt_posterior_density <- function(x, data, depth = 4L) {
  bx <- morie_gh_pt_bits(x, depth)
  n <- length(data)
  counts <- integer(depth)
  for (m in seq_len(depth)) {
    c_ <- 0L
    for (d_ in data) {
      if (identical(morie_gh_pt_bits(d_, m), bx[seq_len(m)])) {
        c_ <- c_ + 1L
      }
    }
    counts[m] <- c_
  }
  dens <- 1
  for (m in seq_len(depth)) {
    a <- m * m
    N_here <- counts[m]
    N_parent <- if (m >= 2) counts[m - 1] else n
    dens <- dens * (2 * a + 2 * N_here) / (2 * a + N_parent)
  }
  dens
}

#' @noRd
morie_gh_hellinger2 <- function(p, q) {
  p <- p / sum(p); q <- q / sum(q)
  1 - sum(sqrt(p * q))
}

#' @noRd
morie_gh_kl <- function(p, q) {
  p <- p / sum(p); q <- q / sum(q)
  keep <- p > 0
  sum(p[keep] * log(p[keep] / pmax(q[keep], 1e-300)))
}

#' @noRd
morie_gh_renyi <- function(p, q, alpha = 0.5) {
  p <- p / sum(p); q <- q / sum(q)
  log(sum(p^alpha * q^(1 - alpha))) / (alpha - 1)
}

#' @noRd
morie_gh_dirichlet_moments <- function(alpha, j, jp) {
  A <- sum(alpha)
  list(mean = alpha[j] / A,
       var = alpha[j] * (A - alpha[j]) / (A^2 * (A + 1)),
       cov = -alpha[j] * alpha[jp] / (A^2 * (A + 1)))
}

#' @noRd
morie_gh_crm_laplace_gamma <- function(f, a) {
  a * log(1 + f)
}

#' @noRd
morie_gh_ncrm_laplace <- function(f, m, u) {
  exp(-sum(m * (1 - exp(-f * u))))
}

#' @noRd
morie_gh_ibp_expected_dishes <- function(n, alpha) {
  alpha * sum(1 / seq_len(n))
}

#' @noRd
morie_gh_dp_predictive <- function(alpha, n) {
  list(weight_fresh = alpha / (alpha + n),
       weight_per_obs = 1 / (alpha + n))
}
