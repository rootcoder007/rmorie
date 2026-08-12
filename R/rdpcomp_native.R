# Renyi DP of the Sampled Gaussian Mechanism.  Source: Mironov, I.,
# Talwar, K. & Zhang, L. (2019), Renyi Differential Privacy of the
# Sampled Gaussian Mechanism, arXiv:1908.10530.  Theorem 4 reduces the
# SGM to a one-dimensional Renyi divergence against the mixture
# (1-q)N(0,s^2) + q N(1,s^2); Corollary 7 makes A_alpha the binding
# direction; Case I expands it binomially for INTEGER alpha, using
#   E_{z~mu0}[(mu1/mu0)^k] = exp((k^2 - k) / (2 s^2)),
# giving the exact finite sum
#   A_alpha = sum_{k=0}^{alpha} C(alpha,k) (1-q)^(alpha-k) q^k
#             exp(k(k-1) / (2 s^2)),      eps(alpha) = log(A) / (alpha-1).
# Composition is Mironov (2017) Proposition 1: the curves add.
# Fractional alpha is the paper's Case II (an infinite series) and is
# NOT implemented; it raises rather than silently misusing Case I.
# Native implementation mirroring Python morie.fn.rdpcomp.

#' Exact RDP of the sampled Gaussian mechanism at an integer order
#'
#' @param alpha Renyi order, an integer greater than 1.
#' @param q Sampling rate in (0, 1].
#' @param sigma Noise multiplier for a sensitivity-1 function.
#' @return The RDP epsilon at \code{alpha}.
#' @references Mironov, Talwar & Zhang (2019), arXiv:1908.10530.
#' @export
morie_rdp_sampled_gaussian <- function(alpha, q, sigma) {
  a <- as.numeric(alpha)[1]
  if (a != floor(a))
    stop("morie_rdp_sampled_gaussian: alpha must be an integer -- the ",
         "closed form is the paper's Case I binomial expansion. ",
         "Fractional orders are Case II, an infinite series, which is ",
         "not implemented")
  ai <- as.integer(a)
  if (ai <= 1) stop("morie_rdp_sampled_gaussian: alpha must exceed 1")
  qq <- as.numeric(q)[1]
  if (!(qq > 0 && qq <= 1))
    stop("morie_rdp_sampled_gaussian: q must lie in (0, 1]")
  s <- as.numeric(sigma)[1]
  if (s <= 0) stop("morie_rdp_sampled_gaussian: sigma must be positive")

  # Summed in log space: exp(k(k-1)/(2 s^2)) reaches exp(alpha^2/(2 s^2)),
  # which overflows a double at the large alpha tight accounting uses.
  log_terms <- numeric(0)
  for (k in 0:ai) {
    lg <- lgamma(ai + 1) - lgamma(k + 1) - lgamma(ai - k + 1)
    if (qq == 1) {
      if (k < ai) next
      lt <- lg + k * log(qq)
    } else if (k == 0) {
      lt <- lg + (ai - k) * log1p(-qq)
    } else {
      lt <- lg + (ai - k) * log1p(-qq) + k * log(qq)
    }
    lt <- lt + k * (k - 1) / (2 * s * s)
    log_terms <- c(log_terms, lt)
  }
  hi <- max(log_terms)
  log_A <- hi + log(sum(exp(log_terms - hi)))
  log_A / (a - 1)
}

#' Compose identical sampled Gaussians (Mironov 2017, Proposition 1)
#'
#' @param alpha Renyi order.
#' @param q Sampling rate.
#' @param sigma Noise multiplier.
#' @param steps Number of composed steps.
#' @return The composed RDP epsilon.
#' @export
morie_rdp_compose <- function(alpha, q, sigma, steps = 1) {
  t <- as.integer(steps)[1]
  if (t < 1) stop("morie_rdp_compose: steps must be at least 1")
  t * morie_rdp_sampled_gaussian(alpha, q, sigma)
}

#' RDP curve of composed sampled Gaussians, optionally converted to DP
#'
#' Mirrors Python \code{morie.fn.rdpcomp}.
#'
#' @param q Sampling rate.
#' @param sigma Noise multiplier.
#' @param alpha Integer orders; defaults to 2:64.
#' @param steps Number of composed steps.
#' @param delta When supplied, also convert via Mironov (2017)
#'   Proposition 3 and minimise over the orders.
#' @return A list with the composed curve and, with \code{delta}, the
#'   converted \code{epsilon} and the \code{best_alpha}.
#' @references Mironov, Talwar & Zhang (2019), arXiv:1908.10530.
#' @export
morie_rdpcomp <- function(q, sigma, alpha = NULL, steps = 1, delta = NULL) {
  orders <- if (is.null(alpha)) 2:64 else as.integer(as.numeric(alpha))
  if (length(orders) < 1)
    stop("morie_rdpcomp: alpha must hold at least one order")
  t <- as.integer(steps)[1]
  curve <- vapply(orders,
                  function(a) morie_rdp_compose(a, q, sigma, steps = t),
                  numeric(1))

  out <- list(estimate = min(curve),
              rdp_epsilons = curve,
              alphas = as.numeric(orders),
              q = as.numeric(q)[1],
              sigma = as.numeric(sigma)[1],
              steps = t,
              method = paste("RDP of the Sampled Gaussian Mechanism",
                             "(Mironov, Talwar & Zhang 2019, Thm 4 /",
                             "Case I); composition by Mironov (2017)",
                             "Prop 1"))
  if (!is.null(delta)) {
    d <- as.numeric(delta)[1]
    if (!(d > 0 && d < 1))
      stop("morie_rdpcomp: delta must lie strictly in (0, 1)")
    eps <- curve + log(1 / d) / (orders - 1)
    best <- 1L
    for (i in seq_along(eps)) if (eps[i] < eps[best]) best <- i
    out$estimate <- eps[best]
    out$epsilon <- eps[best]
    out$best_alpha <- as.numeric(orders[best])
    out$epsilons <- eps
    out$delta <- d
  }
  out
}
