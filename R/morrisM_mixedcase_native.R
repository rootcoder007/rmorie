# morie native arm -- morrisM
# Morris elementary-effects screening.
#
#   EE_i = [ Y(X + Delta e_i) - Y(X) ] / Delta,  Delta = p/(2(p-1))
#
# Reported per factor: mu (mean EE, Morris 1991), sigma (sd of EE,
# which flags interaction/non-linearity), and mu_star (mean |EE|,
# Campolongo et al. 2007). mu_star exists because mu cancels when a
# factor's effect changes sign across the space -- a genuinely
# important factor can show mu near zero.
#
# Costs r(k+1) model runs. All randomness is drawn as sequential
# scalar uniforms in the same order as the Python, so the two agree
# trajectory for trajectory.
#
# Morris (1991) Technometrics 33, 161-174; Campolongo, Cariboni &
# Saltelli (2007) Env. Modelling & Software 22, 1509-1518;
# Saltelli et al. (2008) GSA Primer Sec. 3.2-3.3.

#' morie_morrisM
#'
#' Part of the morrisM_mixedcase_native implementation; see the file
#' header for the source it follows.
#'
#' @param fun See Usage.
#' @param k See Usage.
#' @param r Defaults to \code{10}.
#' @param p Defaults to \code{4}.
#' @param seed Defaults to \code{0}.
#' @param bounds Defaults to \code{NULL}.
#' @return A list with \code{estimate}, \code{mu}, \code{mu_star}, \code{sigma}, \code{elementary_effects}, \code{n_runs}, \code{delta}, \code{n_levels}, \code{r}, \code{p}, \code{k}, \code{method}.
#' @export
morie_morrisM <- function(fun, k, r = 10, p = 4, seed = 0,
                          bounds = NULL) {
  k <- as.integer(k); r <- as.integer(r); p <- as.integer(p)
  if (is.null(bounds)) {
    bounds <- lapply(seq_len(k), function(i) c(0, 1))
  }
  if (length(bounds) != k ||
      any(vapply(bounds, function(b) b[2] <= b[1], logical(1)))) {
    stop("bounds must be k pairs with low < high")
  }
  delta <- p / (2 * (p - 1))
  lv <- (seq_len(p) - 1) / (p - 1)
  levels <- lv[lv <= 1 - delta + 1e-12]
  nlev <- length(levels)

  e <- .ghc_rng(seed)
  unif <- function() .ghc_unif(e, 1L)
  scale_x <- function(u) {
    vapply(seq_len(k), function(i)
      bounds[[i]][1] + u[i] * (bounds[[i]][2] - bounds[[i]][1]),
      numeric(1))
  }

  ee <- vector("list", k)
  for (i in seq_len(k)) ee[[i]] <- numeric(0)
  n_runs <- 0L

  for (traj in seq_len(r)) {
    x <- vapply(seq_len(k), function(i)
      levels[min(as.integer(unif() * nlev), nlev - 1L) + 1L],
      numeric(1))
    keys <- vapply(seq_len(k), function(i) unif(), numeric(1))
    order_i <- order(keys)
    y <- as.numeric(fun(scale_x(x)))
    n_runs <- n_runs + 1L
    for (i in order_i) {
      step <- if (unif() < 0.5) delta else -delta
      if (x[i] + step > 1 + 1e-12 || x[i] + step < -1e-12) {
        step <- -step
      }
      x2 <- x; x2[i] <- x[i] + step
      y2 <- as.numeric(fun(scale_x(x2)))
      n_runs <- n_runs + 1L
      ee[[i]] <- c(ee[[i]], (y2 - y) / step)
      x <- x2; y <- y2
    }
  }
  mu <- vapply(ee, mean, numeric(1))
  mu_star <- vapply(ee, function(v) mean(abs(v)), numeric(1))
  sigma <- vapply(seq_along(ee), function(i) {
    v <- ee[[i]]
    if (length(v) > 1L) sd(v) else 0
  }, numeric(1))
  list(
    estimate = mu_star, mu = mu, mu_star = mu_star, sigma = sigma,
    elementary_effects = ee, n_runs = n_runs,
    delta = delta, n_levels = nlev, r = r, p = p, k = k,
    method = paste0("Morris elementary effects (Morris 1991; ",
                    "mu* per Campolongo et al. 2007)")
  )
}

# The 2026-08-11 arm of this module was a second
# implementation of the same paper; it has been removed and
# its exported name kept as an alias. The formals were
# identical, so this is exact and the man page still applies.
morie_morrism <- morie_morrisM
