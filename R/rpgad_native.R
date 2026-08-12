# Renyi differential privacy and its conversion to (eps, delta)-DP.
# Source: Mironov, I. (2017), Renyi Differential Privacy, 30th IEEE
# Computer Security Foundations Symposium (CSF), 263-275.
#   Proposition 3:  (alpha, eps)-RDP => (eps + log(1/delta)/(alpha-1), delta)-DP
#   Proposition 1:  RDP curves ADD under (adaptive) composition
#   Proposition 7:  D_alpha(N(0,s^2) || N(mu,s^2)) = alpha mu^2 / (2 s^2)
#   Corollary 3:    sensitivity-1 Gaussian is (alpha, alpha/(2 sigma^2))-RDP
#   Corollary 2:    sensitivity-1 Laplace RDP curve (see below)
# Native implementation mirroring Python morie.fn.rpgad, loop for loop.

#' Gaussian RDP curve (Mironov 2017, Corollary 3)
#'
#' @param alpha Renyi order.
#' @param sigma Noise scale.
#' @param sensitivity Query sensitivity.
#' @return The RDP epsilon at \code{alpha}.
#' @export
morie_rdp_gaussian <- function(alpha, sigma, sensitivity = 1) {
  a <- as.numeric(alpha)[1]
  s <- as.numeric(sigma)[1]
  d <- as.numeric(sensitivity)[1]
  if (s <= 0) stop("morie_rdp_gaussian: sigma must be positive")
  if (d < 0) stop("morie_rdp_gaussian: sensitivity must be non-negative")
  a * d * d / (2 * s * s)
}

#' Laplace RDP curve (Mironov 2017, Corollary 2)
#'
#' Evaluated in log space: the first term carries exp((alpha-1)/lambda),
#' which overflows for large alpha -- and large alpha is exactly where
#' the curve is read, since the epsilon tends to the pure-DP 1/lambda.
#'
#' @param alpha Renyi order, must exceed 1.
#' @param lam Noise scale.
#' @param sensitivity Query sensitivity.
#' @return The RDP epsilon at \code{alpha}.
#' @export
morie_rdp_laplace <- function(alpha, lam, sensitivity = 1) {
  a <- as.numeric(alpha)[1]
  sens <- as.numeric(sensitivity)[1]
  lm <- if (sens != 0) as.numeric(lam)[1] / sens else Inf
  if (a <= 1) stop("morie_rdp_laplace: alpha must exceed 1")
  if (lm <= 0) stop("morie_rdp_laplace: lambda must be positive")
  log_t1 <- log(a / (2 * a - 1)) + (a - 1) / lm
  log_t2 <- log((a - 1) / (2 * a - 1)) - a / lm
  hi <- max(log_t1, log_t2)
  lo <- min(log_t1, log_t2)
  (hi + log1p(exp(lo - hi))) / (a - 1)
}

#' Convert an RDP curve to (epsilon, delta)-differential privacy
#'
#' Applies Mironov (2017) Proposition 3 at every supplied Renyi order
#' and returns the minimum, since the bound holds for all of them.
#' Mirrors Python \code{morie.fn.rpgad}.
#'
#' @param alpha One Renyi order or several; every one must exceed 1.
#' @param epsilon_R RDP epsilon at each order; omit and give
#'   \code{mechanism} to use a closed-form curve instead.
#' @param delta Target delta, strictly inside (0, 1).
#' @param mechanism Either \code{"gaussian"} (Corollary 3) or
#'   \code{"laplace"} (Corollary 2).
#' @param sigma Noise scale for the Gaussian mechanism.
#' @param lam Noise scale for the Laplace mechanism.
#' @param sensitivity Query sensitivity.
#' @param n_compositions Number of identical mechanisms composed;
#'   Proposition 1 makes the curves add.
#' @return A list with the best \code{epsilon}, the \code{best_alpha}
#'   attaining it, and the per-order curves.
#' @references Mironov, I. (2017). Renyi differential privacy. 30th IEEE
#'   Computer Security Foundations Symposium, 263-275.
#' @export
morie_rpgad <- function(alpha, epsilon_R = NULL, delta = 1e-5,
                        mechanism = NULL, sigma = NULL, lam = NULL,
                        sensitivity = 1, n_compositions = 1) {
  orders <- as.numeric(alpha)
  if (length(orders) < 1)
    stop("morie_rpgad: alpha must hold at least one order")
  if (any(orders <= 1))
    stop("morie_rpgad: every alpha must exceed 1 (Proposition 3 divides ",
         "by alpha - 1)")
  d <- as.numeric(delta)[1]
  if (!(d > 0 && d < 1))
    stop("morie_rpgad: delta must lie strictly in (0, 1)")
  k <- as.integer(n_compositions)[1]
  if (k < 1) stop("morie_rpgad: n_compositions must be at least 1")

  if (!is.null(epsilon_R)) {
    eps_r <- as.numeric(epsilon_R)
    if (length(eps_r) == 1 && length(orders) > 1)
      eps_r <- rep(eps_r, length(orders))
    if (length(eps_r) != length(orders))
      stop("morie_rpgad: got ", length(orders), " alpha but ",
           length(eps_r), " epsilon_R")
    mech <- "supplied"
  } else {
    if (is.null(mechanism))
      stop("morie_rpgad: give either epsilon_R or a mechanism")
    mech <- tolower(as.character(mechanism)[1])
    if (!mech %in% c("gaussian", "laplace"))
      stop("morie_rpgad: mechanism must be gaussian or laplace")
    if (mech == "gaussian") {
      if (is.null(sigma)) stop("morie_rpgad: mechanism='gaussian' needs sigma")
      eps_r <- vapply(orders,
                      function(a) morie_rdp_gaussian(a, sigma, sensitivity),
                      numeric(1))
    } else {
      if (is.null(lam)) stop("morie_rpgad: mechanism='laplace' needs lam")
      eps_r <- vapply(orders,
                      function(a) morie_rdp_laplace(a, lam, sensitivity),
                      numeric(1))
    }
  }

  eps_r <- k * eps_r                       # Proposition 1: curves add
  eps <- eps_r + log(1 / d) / (orders - 1)  # Proposition 3

  best <- 1L
  for (i in seq_along(eps)) if (eps[i] < eps[best]) best <- i

  list(estimate = eps[best],
       epsilon = eps[best],
       best_alpha = orders[best],
       epsilons = eps,
       alphas = orders,
       rdp_epsilons = eps_r,
       delta = d,
       mechanism = mech,
       sensitivity = as.numeric(sensitivity)[1],
       n_compositions = k,
       method = "RDP -> (eps, delta)-DP, Mironov (2017) Proposition 3")
}
