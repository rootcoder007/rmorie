# SPDX-License-Identifier: AGPL-3.0-or-later

# Negative log-likelihood of the GPD at (sigma, xi) for excesses y.
#' Negative log-likelihood of the GPD at (sigma, xi) for excesses y
#'
#' A step of the evpot implementation. Called by \code{Evpot}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param y A vector; its length is taken.
#' @param sigma Numeric; passed to \code{log}.
#' @param xi Numeric; passed to \code{abs}.
#' @return A numeric value.
#' @export
.gpd_nll <- function(y, sigma, xi) {
  n <- length(y)
  if (sigma <= 0) return(Inf)
  if (abs(xi) < 1e-12) return(n * log(sigma) + sum(y) / sigma)
  s <- 0
  for (v in y) {
    z <- 1 + xi * v / sigma
    if (z <= 0) return(Inf)
    s <- s + log(z)
  }
  n * log(sigma) + (1 / xi + 1) * s
}

# Grimshaw's reparametrisation: given t = xi/sigma, xi and sigma follow.
#' Grimshaw\'s reparametrisation: given t = xi/sigma, xi and sigma
#' follow
#'
#' A step of the evpot implementation. Called by \code{Evpot}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param y A vector; its length is taken.
#' @param t Numeric; passed to \code{abs}.
#' @return A list with \code{xi}, \code{sigma}, \code{g}.
#' @export
.gpd_profile <- function(y, t) {
  n <- length(y)
  s <- 0
  for (v in y) {
    z <- 1 + t * v
    if (z <= 0) return(NULL)
    s <- s + log(z)
  }
  xi <- s / n
  if (xi == 0) return(NULL)
  list(xi = xi, sigma = xi / t, g = log(abs(t)) - log(abs(xi)) - xi)
}

#' Peaks-over-threshold GPD fit + scale-invariance check
#'
#' Formula: fit GPD on x\[x>u\]; rate zeta_u = N_u/n
#'
#' Maximum likelihood in Grimshaw's one-parameter reduction: with
#' t = xi/sigma the profile has xi(t) = mean(log(1 + t y)) and
#' sigma(t) = xi(t)/t, leaving a single bounded search.  The exponential
#' (xi = 0) fit, sigma = mean(y), is evaluated separately and kept if
#' its likelihood is higher.  The modified scale sigma - xi u is
#' reported: it is invariant to the threshold when the GPD model holds.
#'
#' @param x Sample.
#' @param u Threshold.
#' @return List with \code{sigma}, \code{xi}, \code{zeta_u},
#'   \code{estimate}, \code{n_exceed}, \code{n}, \code{nll},
#'   \code{modified_scale}, \code{method}.
#' @references Davison & Smith (1990), JRSS B 52(3):393-442;
#'   Grimshaw (1993), Technometrics 35(2):185-191.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
#' Evpot(V, M)
Evpot <- function(x, u) {
  x <- .s03vec(x)
  n <- length(x)
  if (n == 0L) stop("empty input: x has no observations")
  u <- as.numeric(u)
  y <- sort(x[x > u] - u)
  k <- length(y)
  if (k < 2L) stop("fewer than two exceedances above u; nothing to fit")
  ymax <- y[k]
  ybar <- sum(y) / k
  best <- list(nll = .gpd_nll(y, ybar, 0), sigma = ybar, xi = 0)
  lo <- -1 / ymax + 1e-10
  hi <- 4 / ybar
  grid <- 4000L
  gbest <- -Inf; tbest <- NA_real_
  for (i in 0:grid) {
    t <- lo + (hi - lo) * i / grid
    if (abs(t) < 1e-12) next
    pr <- .gpd_profile(y, t)
    if (is.null(pr)) next
    if (pr$g > gbest || (pr$g == gbest && !is.na(tbest) && t > tbest)) {
      gbest <- pr$g; tbest <- t
    }
  }
  if (!is.na(tbest)) {
    step <- (hi - lo) / grid
    a <- tbest - step; b <- tbest + step
    gr <- 0.5 * (sqrt(5) - 1)
    for (it in seq_len(200)) {
      cc <- b - gr * (b - a)
      dd <- a + gr * (b - a)
      pc <- .gpd_profile(y, cc)
      pd <- .gpd_profile(y, dd)
      fc <- if (is.null(pc)) -1e300 else pc$g
      fd <- if (is.null(pd)) -1e300 else pd$g
      if (fc > fd) b <- dd else a <- cc
    }
    t <- 0.5 * (a + b)
    pr <- .gpd_profile(y, t)
    if (!is.null(pr)) {
      nll <- .gpd_nll(y, pr$sigma, pr$xi)
      if (nll < best$nll) best <- list(nll = nll, sigma = pr$sigma, xi = pr$xi)
    }
  }
  .t1_result(sigma = best$sigma, xi = best$xi, zeta_u = k / n,
             estimate = best$xi, n_exceed = k, n = n, nll = best$nll,
             modified_scale = best$sigma - best$xi * u,
             method = "GPD maximum likelihood on threshold exceedances")
}
