# SPDX-License-Identifier: AGPL-3.0-or-later

#' Fauzi: Higher-order (order-4) Gaussian-based kernel (Ch 1)
#'
#' KDE with the Gaussian-based kernel of even order 2r,
#' phi(u) sum_(j<r) (-1)^j He_2j(u) / (2^j j!), e.g. the order-4 Wand-Jones (1995, eq 2.8) kernel
#' \eqn{K_4(u) = (1/2)(3-u^2)\phi(u)}{K_4(u) = (1/2)(3-u^2)phi(u)}.  Bias reduces from O(h^2)
#' to O(h^4).  Note: K_4 takes negative values so f_hat may be < 0.
#'
#' @param x Numeric vector.
#' @param t Evaluation point; default median(x).
#' @param h Bandwidth; default = Silverman.
#' @param order Even kernel order (2 is the Gaussian, 4 the Wand-Jones K_4).
#' @return Named list with estimate, h, t, order, mu_r, R_K, n, method.
#' @importFrom stats median dnorm
#' @examples
#' set.seed(1)
#' fzhok(x = rnorm(50))
#' @export
fzhok <- function(x, t = NULL, h = NULL, order = 4L) {
  x <- as.numeric(x)
  n <- length(x)
  if (n < 2L) {
    return(list(
      estimate = NA_real_, n = n,
      method = "fzhok - too few obs"
    ))
  }
  cf <- .fzhok_poly(order)
  if (is.null(t)) t <- stats::median(x)
  if (is.null(h)) h <- .morie_silverman_h(x)
  u <- (t - x) / h
  k <- vapply(u, function(v) sum(cf * v^(seq_along(cf) - 1)), 0) * exp(-0.5 * u * u) / sqrt(2 * pi)
  f_hat <- sum(k) / (n * h)
  m <- .fzhok_moments(cf, order)
  list(
    estimate = f_hat, h = h, t = t, order = order,
    mu_r = m[1], R_K = m[2], n = n,
    method = paste0("Fauzi higher-order (", order, ") Gaussian-based kernel density (Ch 1)")
  )
}

.fzhok_poly <- function(order) {
  if (order < 2 || order %% 2 != 0) stop("order must be an even integer >= 2")
  cf <- numeric(order)
  for (j in 0:(order / 2 - 1)) {
    s <- (-1)^j / (2^j * factorial(j))
    for (k in 0:j) {
      cf[2 * j - 2 * k + 1] <- cf[2 * j - 2 * k + 1] + s * (-1)^k * factorial(2 * j) /
        (factorial(k) * factorial(2 * j - 2 * k) * 2^k)
    }
  }
  cf
}

.fzhok_moments <- function(cf, order) {
  dfact <- function(m) if (m <= 0) 1 else prod(seq(m - 1, 1, by = -2))
  m <- seq_along(cf) - 1
  mu <- sum(vapply(m, function(a) if ((order + a) %% 2 == 0) cf[a + 1] * dfact(order + a) else 0, 0))
  rk <- 0
  for (a in m) for (b in m) if ((a + b) %% 2 == 0) rk <- rk + cf[a + 1] * cf[b + 1] * gamma((a + b + 1) / 2) / (2 * pi)
  c(mu, rk)
}

# CANONICAL TEST
# set.seed(0); x <- rnorm(2000); r <- fzhok(x, t = 0)
# stopifnot(abs(r$estimate - dnorm(0)) < 0.1)

#' @rdname fzhok
#' @keywords internal
#' @export
morie_fauzi_higher_order_kernel <- fzhok
