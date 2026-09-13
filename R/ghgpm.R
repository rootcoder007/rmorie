# SPDX-License-Identifier: AGPL-3.0-or-later

#' GP posterior mean with Matern kernel
#'
#' @param x Numeric vector or matrix of input points.
#' @param y Numeric response vector.
#' @param nu Matern smoothness parameter (default 1.5).
#' @param length_scale Optional kernel length-scale.
#' @param sigma_f Numeric signal sd (default 1).
#' @param noise Optional observation noise sd.
#' @param x_star Optional matrix of prediction points (defaults to x).
#' @return Named list with estimate, se, mu, sd, length_scale, nu, noise, n, method.
#' @examples
#' set.seed(1)
#' morie_ghosal_gp_matern(x = rnorm(50), y = rnorm(50))
#' @export
morie_ghosal_gp_matern <- function(x, y, nu = 1.5, length_scale = NULL,
                                   sigma_f = 1.0, noise = NULL, x_star = NULL) {
  if (is.vector(x)) x <- matrix(as.numeric(x), ncol = 1L) else x <- as.matrix(x)
  y <- as.numeric(y)
  n <- nrow(x)
  if (is.null(x_star)) {
    x_star <- x
  } else if (is.vector(x_star)) {
    x_star <- matrix(as.numeric(x_star), ncol = 1L)
  } else {
    x_star <- as.matrix(x_star)
  }
  sq <- pmax(.gh_pairwise_sq(x), 0)
  if (is.null(length_scale)) {
    d <- sqrt(sq[upper.tri(sq)])
    length_scale <- if (length(d)) max(stats::median(d[d > 0]), 1e-3) else 1
  }
  if (is.null(noise)) noise <- max(0.1 * stats::sd(y), 1e-3)
  kernel <- function(a, b) {
    sq_ab <- pmax(.gh_pairwise_sq(a, b), 0)
    r <- sqrt(sq_ab)
    if (isTRUE(all.equal(nu, 0.5))) {
      return(sigma_f^2 * exp(-r / length_scale))
    }
    if (isTRUE(all.equal(nu, 1.5))) {
      t <- sqrt(3) * r / length_scale
      return(sigma_f^2 * (1 + t) * exp(-t))
    }
    if (isTRUE(all.equal(nu, 2.5))) {
      t <- sqrt(5) * r / length_scale
      return(sigma_f^2 * (1 + t + t^2 / 3) * exp(-t))
    }
    rr <- pmax(r, 1e-12)
    z <- sqrt(2 * nu) * rr / length_scale
    coef <- sigma_f^2 * 2^(1 - nu) / gamma(nu)
    K <- coef * z^nu * besselK(z, nu)
    K[r < 1e-12] <- sigma_f^2
    K
  }
  K <- kernel(x, x) + noise^2 * diag(n)
  K_s <- kernel(x_star, x)
  K_ss_diag <- rep(sigma_f^2, nrow(x_star))
  ## A fixed absolute jitter is the wrong scale: it has to be small
  ## RELATIVE to the kernel's own magnitude, not small in absolute
  ## terms. With sigma_f and the data's spread free, K can be large
  ## enough that 1e-8 is no nudge at all, and the factorisation fails --
  ## "the leading minor of order 20 is not positive" on R 4.4 with a
  ## different BLAS, where it had succeeded elsewhere. So the jitter is
  ## scaled to the mean diagonal and escalated until the factorisation
  ## takes, rather than fixed at a value chosen in advance.
  ## A kernel matrix is symmetric by construction, so say so: the Gram
  ## form above is only symmetric up to rounding, and the factorisation
  ## reads one triangle.
  K <- (K + t(K)) / 2
  if (!all(is.finite(K))) {
    stop("the kernel matrix is not finite (", sum(!is.finite(K)),
         " of ", length(K), " entries): check `length_scale` (",
         format(length_scale, digits = 3), ") and `nu` (",
         format(nu, digits = 3), ") against the scale of `x`",
         call. = FALSE)
  }
  jitter0 <- 1e-8 * max(mean(diag(K)), .Machine$double.eps)
  L <- NULL
  last_err <- NULL
  for (k in 0:8) {
    L <- tryCatch(chol(K + (jitter0 * 10^k) * diag(n)),
                  error = function(e) {
                    last_err <<- conditionMessage(e)
                    NULL
                  })
    if (!is.null(L)) break
  }
  if (is.null(L)) {
    ## Report what the factorisation actually said. The previous message
    ## guessed at duplicated rows, which sent the diagnosis in the wrong
    ## direction when the cause was elsewhere.
    stop("the kernel matrix could not be factorised even with a jitter of ",
         format(jitter0 * 1e8, digits = 3), " (mean diagonal ",
         format(mean(diag(K)), digits = 3), ", n = ", n, "): ",
         if (is.null(last_err)) "no error was reported" else last_err,
         call. = FALSE)
  }
  alpha_ <- backsolve(L, forwardsolve(t(L), y))
  mu <- as.numeric(K_s %*% alpha_)
  v <- forwardsolve(t(L), t(K_s))
  var <- K_ss_diag - colSums(v^2)
  sd_ <- sqrt(pmax(var, 0))
  list(
    estimate = mean(mu), se = mean(sd_), mu = mu, sd = sd_,
    length_scale = length_scale, nu = nu, noise = noise, n = n,
    method = "GP regression (Matern kernel)"
  )
}
