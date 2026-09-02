# SPDX-License-Identifier: AGPL-3.0-or-later

#' ETS state-space (error/trend/seasonal)
#'
#' Formula: the additive-error innovations state space form
#' \preformatted{
#'   y_t = l_{t-1} + b_{t-1} + s_{t-m} + e_t
#'   l_t = l_{t-1} + b_{t-1} + alpha e_t
#'   b_t = b_{t-1}           + beta  e_t
#'   s_t = s_{t-m}           + gamma e_t
#' }
#' i.e. y_t = w' x_{t-1} + e_t, x_t = F x_{t-1} + g e_t.  Smoothing
#' parameters are chosen on a deterministic 0.1-step grid minimising the
#' one-step sum of squared errors, or may be fixed directly.
#'
#' @param y Series.
#' @param error Error type; only "A" (additive) is implemented.
#' @param trend Include the additive local trend b_t.
#' @param season Seasonal period m; 0 or 1 means no seasonal component.
#' @param alpha,beta,gamma Fixed smoothing parameters (bypass the grid).
#' @return List with \code{estimate}, \code{alpha}, \code{beta},
#'   \code{gamma}, \code{sse}, \code{sigma2}, \code{aic}, \code{level},
#'   \code{slope}, \code{forecast}, \code{n}, \code{method}.
#' @references Hyndman, Koehler, Snyder & Grose (2002), International
#'   Journal of Forecasting 18(3):439-454,
#'   doi:10.1016/S0169-2070(01)00110-8.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Etsmod(V)
Etsmod <- function(y, error = "A", trend = FALSE, season = 0,
                   alpha = NULL, beta = NULL, gamma = NULL) {
  y <- as.numeric(y)
  n <- length(y)
  if (n == 0L) stop("empty input: y has no observations")
  if (toupper(as.character(error)) != "A")
    stop("only the additive error form 'A' is implemented")
  m <- as.integer(season)
  if (m < 0L) stop("season must be non-negative")
  if (m <= 1L) m <- 0L
  if (m > 0L && n < 2L * m)
    stop(sprintf("need at least two full seasons for season = %d", m))
  use_b <- isTRUE(as.logical(trend))
  .ini <- function() {
    if (m > 0L) {
      l0 <- sum(y[seq_len(m)]) / m
      b0 <- if (use_b) ((sum(y[(m + 1L):(2L * m)]) / m) - l0) / m else 0
      s0 <- y[seq_len(m)] - l0
    } else {
      l0 <- y[1]
      b0 <- if (use_b && n > 1L) y[2] - y[1] else 0
      s0 <- numeric(0)
    }
    list(l0, b0, s0)
  }
  .sse <- function(a, b, g) {
    ini <- .ini()
    l <- ini[[1]]; bt <- ini[[2]]; s <- ini[[3]]
    tot <- 0
    for (t in seq_len(n)) {
      idx <- if (m > 0L) ((t - 1L) %% m) + 1L else 0L
      sea <- if (m > 0L) s[idx] else 0
      e <- y[t] - (l + bt + sea)
      tot <- tot + e * e
      lnew <- l + bt + a * e
      if (use_b) bt <- bt + b * e
      if (m > 0L) s[idx] <- sea + g * e
      l <- lnew
    }
    list(tot, l, bt, s)
  }
  grid <- (1:9) / 10
  A <- if (!is.null(alpha)) as.numeric(alpha) else grid
  best <- NULL
  for (a in A) {
    Bs <- if (!is.null(beta)) as.numeric(beta) else if (use_b) grid[grid <= a] else 0
    for (b in Bs) {
      Gs <- if (!is.null(gamma)) as.numeric(gamma) else if (m > 0L) grid[grid <= 1 - a] else 0
      for (g in Gs) {
        tot <- .sse(a, b, g)[[1]]
        if (is.null(best) || tot < best[1] - 1e-15) best <- c(tot, a, b, g)
      }
    }
  }
  sse <- best[1]; a <- best[2]; b <- best[3]; g <- best[4]
  fin <- .sse(a, b, g)
  level <- fin[[2]]; slope <- fin[[3]]; s <- fin[[4]]
  k <- 1L + as.integer(use_b) + as.integer(m > 0L) + 1L + as.integer(use_b) + m
  sigma2 <- sse / n
  aic <- if (sse > 0) n * log(sse / n) + 2 * k else -Inf
  fc <- level + slope + (if (m > 0L) s[(n %% m) + 1L] else 0)
  .t1_result(estimate = fc, alpha = a, beta = b, gamma = g, sse = sse,
             sigma2 = sigma2, aic = aic, level = level, slope = slope,
             forecast = fc, n = n,
             method = "ETS state-space (error/trend/seasonal)")
}
