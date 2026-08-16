# Chambolle-Pock primal-dual hybrid gradient.
#
# Chambolle, A., & Pock, T. (2011) "A First-Order Primal-Dual Algorithm
# for Convex Problems with Applications to Imaging", *Journal of
# Mathematical Imaging and Vision* **40**(1), 120-145.
#
# Solves the saddle-point problem
#
# .. math:: \min_x \max_y \; \langle Kx, y\rangle + G(x) - F^*(y)
#
# by Algorithm 1 (their eq. 8), alternating a dual ascent, a primal
# descent, and an extrapolation:
#
# .. math::
#     y^{n+1} &= \mathrm{prox}_{\sigma F^*}(y^n + \sigma K \bar{x}^n)\\
#     x^{n+1} &= \mathrm{prox}_{\tau G}(x^n - \tau K^* y^{n+1})\\
#     \bar{x}^{n+1} &= x^{n+1} + \theta (x^{n+1} - x^n)
#
# The step sizes must satisfy :math:`\tau\sigma\lVert K\rVert^2 < 1`
# (their Theorem 1); with :math:`\theta = 1` this converges at
# :math:`O(1/N)` on the partial primal-dual gap. The condition is
# enforced here rather than assumed, because violating it does not
# produce a warning -- it produces a divergent sequence that still
# returns numbers.
#
# The extrapolation :math:`\bar{x}` is the whole trick: with
# :math:`\theta = 0` the method reduces to plain Arrow-Hurwicz, which is
# not convergent under these step sizes.
#
# Routes
# ------
# ``theta`` exposes the relaxation: 1 is the paper's convergent choice, 0
# recovers Arrow-Hurwicz. ``prox_f_star`` and ``prox_g`` are supplied by
# the caller, so any :math:`F, G` pair works; :func:`tv_denoise_1d` wires
# up the paper's own example, total-variation denoising, where
# :math:`F^*` is the projection onto the :math:`\ell_\infty` ball.

#' morie_primal
#'
#' A step of the primal_native implementation. Called by \code{morie_tv_denoise_1d}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param K Accepted by the signature and not used anywhere in the body.
#' @param Kt Accepted by the signature and not used anywhere in the body.
#' @param prox_f_star Accepted by the signature and not used anywhere in the body.
#' @param prox_g Accepted by the signature and not used anywhere in the body.
#' @param x0 Coerced to numeric by the body, with \code{as.numeric}.
#' @param y0 Coerced to numeric by the body, with \code{as.numeric}.
#' @param tau Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.
#' @param sigma Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.
#' @param theta Numeric; combined arithmetically in the body. Defaults to \code{1}.
#' @param norm_K Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.
#' @param max_iter Coerced to integer by the body, with \code{as.integer}. Defaults to \code{500}.
#' @param tol Defaults to \code{1e-10}.
#' @return The value of \code{result}, as built in the body.
#' @export
morie_primal <- function(K, Kt, prox_f_star, prox_g, x0, y0,
                         tau = NULL, sigma = NULL,
                         theta = 1.0, norm_K = NULL,
                         max_iter = 500, tol = 1e-10) {
  x <- as.numeric(x0)
  y <- as.numeric(y0)
  theta <- as.numeric(theta)

  # Power iteration on K*K to bound ||K||.
  if (is.null(norm_K)) {
    v <- rep(1.0, length(x))
    nrm <- 1.0
    for (iter in seq_len(100)) {
      w <- as.numeric(Kt(K(v)))
      nrm2 <- sqrt(sum(w * w))
      if (nrm2 <= 0.0) {
        break
      }
      v <- w / nrm2
      nrm <- sqrt(nrm2)
    }
    norm_K <- max(nrm, 1e-12)
  }
  norm_K <- as.numeric(norm_K)

  if (is.null(tau)) {
    tau <- 1.0 / norm_K
  }
  if (is.null(sigma)) {
    sigma <- 1.0 / norm_K
  }
  tau <- as.numeric(tau)
  sigma <- as.numeric(sigma)

  if (tau <= 0.0 || sigma <= 0.0) {
    stop(sprintf("chambolle_pock: tau and sigma must be positive, got %g and %g",
                 tau, sigma))
  }

  prod <- tau * sigma * norm_K * norm_K
  if (prod >= 1.0 + 1e-12) {
    stop(sprintf("chambolle_pock: Theorem 1 requires tau*sigma*||K||^2 <= 1, got %.6g. The iteration diverges outside this range while still returning finite numbers, so this is refused rather than warned about.",
                 prod))
  }

  xbar <- x
  it <- 0L
  converged <- FALSE

  for (it in seq_len(as.integer(max_iter))) {
    Kx <- as.numeric(K(xbar))
    y_input <- y + sigma * Kx
    y <- as.numeric(prox_f_star(y_input, sigma))

    Kty <- as.numeric(Kt(y))
    x_input <- x - tau * Kty
    x_new <- as.numeric(prox_g(x_input, tau))

    xbar <- x_new + theta * (x_new - x)

    step <- sqrt(sum((x_new - x)^2))
    x <- x_new

    if (step <= tol) {
      converged <- TRUE
      break
    }
  }

  result <- list(
    estimate = x,
    x = x,
    y = y,
    tau = tau,
    sigma = sigma,
    theta = theta,
    norm_K = norm_K,
    step_condition = prod,
    iterations = as.integer(it),
    converged = converged,
    method = "Chambolle-Pock primal-dual hybrid gradient (Chambolle & Pock 2011, Algorithm 1)"
  )

  return(result)
}

#' morie_tv_denoise_1d
#'
#' A step of the primal_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param signal Coerced to numeric by the body, with \code{as.numeric}.
#' @param lam Numeric; combined arithmetically in the body. Defaults to \code{1}.
#' @param max_iter Passed to \code{morie_primal}. Defaults to \code{1000}.
#' @param tol Passed to \code{morie_primal}. Defaults to \code{1e-12}.
#' @param theta Passed to \code{morie_primal}. Defaults to \code{1}.
#' @return The value of \code{res}, as built in the body.
#' @export
morie_tv_denoise_1d <- function(signal, lam = 1.0, max_iter = 1000,
                                tol = 1e-12, theta = 1.0) {
  b <- as.numeric(signal)
  n <- length(b)
  if (n < 2) {
    stop("tv_denoise_1d: need at least two samples")
  }
  lam <- as.numeric(lam)
  if (lam < 0.0) {
    stop("tv_denoise_1d: lam must be non-negative")
  }

  K <- function(x) {
    out <- numeric(n - 1)
    for (i in seq_len(n - 1)) {
      out[i] <- x[i + 1] - x[i]
    }
    out
  }

  Kt <- function(y) {
    out <- rep(0.0, n)
    for (i in seq_len(n - 1)) {
      out[i] <- out[i] - y[i]
      out[i + 1] <- out[i + 1] + y[i]
    }
    out
  }

  prox_fs <- function(y, s) {
    pmin(pmax(y, -lam), lam)
  }

  prox_g <- function(x, t) {
    (x + t * b) / (1.0 + t)
  }

  res <- morie_primal(K, Kt, prox_fs, prox_g, b, rep(0.0, n - 1),
                      theta = theta, norm_K = 2.0,
                      max_iter = max_iter, tol = tol)

  res$lambda <- lam
  res$signal <- b

  Kx <- K(res$x)
  res$objective <- 0.5 * sum((res$x - b)^2) + lam * sum(abs(Kx))

  return(res)
}

#' .primal_morie_cheatsheet
#'
#' A step of the primal_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.primal_morie_cheatsheet <- function() {
  return(paste("primal: Chambolle-Pock, y = prox_{s F*}(y + s K xbar),",
               "x = prox_{t G}(x - t K* y), xbar = x + theta (x - x_prev);",
               "requires tau sigma ||K||^2 < 1."))
}

# Aliases
morie_primal_dual <- morie_primal
morie_primaldual <- morie_primal
