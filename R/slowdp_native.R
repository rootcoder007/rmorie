# morie.fn -- function file (rootcoder007/morie)
# R arm of slowdp (stick_breaking, truncation_error, ...).
# Sources:
#   Sethuraman, J. (1994) "A Constructive Definition of Dirichlet
#   Priors", Statistica Sinica 4(2), 639-650. The stick-breaking
#   construction: independent Beta(1, alpha) variables V_k and
#   locations from the base measure give
#   p_k = V_k prod_{l<k}(1 - V_l) with sum p_k = 1 almost surely.
#   Ferguson, T. S. (1973) "A Bayesian Analysis of Some Nonparametric
#   Problems", The Annals of Statistics 1(2), 209-230,
#   doi:10.1214/aos/1176342360. The Dirichlet process itself.
#   Ishwaran, H. & James, L. F. (2001) "Gibbs Sampling Methods for
#   Stick-Breaking Priors", JASA 96(453), 161-173,
#   doi:10.1198/016214501750332758. Truncated stick-breaking analysis.
#   Neal, R. M. (2000) "Markov Chain Sampling Methods for Dirichlet
#   Process Mixture Models", JCGS 9(2), 249-265,
#   doi:10.1080/10618600.2000.10474879. The sampling context in which
#   a truncation level has to be chosen.

#' .slowdp_beta1alpha
#'
#' A step of the slowdp_native implementation. Called by \code{stick_breaking}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param e Passed to \code{.ghc_unif}.
#' @param alpha Coerced to numeric by the body, with \code{as.numeric}.
#' @return A numeric value.
#' @export
.slowdp_beta1alpha <- function(e, alpha) {
  u <- .ghc_unif(e, 1L)
  u <- min(max(u, 1e-15), 1.0 - 1e-15)
  1.0 - u ^ (1.0 / as.numeric(alpha))
}

#' stick_breaking
#'
#' A step of the slowdp_native implementation. Called by \code{truncated_dp}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param alpha Coerced to numeric by the body, with \code{as.numeric}.
#' @param K Coerced to integer by the body, with \code{as.integer}.
#' @param rng Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @param seed Passed to \code{.ghc_rng}. Defaults to \code{0}.
#' @return A list with \code{weights}, \code{V}, \code{remaining}, \code{kept_mass}, \code{K}, \code{alpha}, \code{note}.
#' @export
stick_breaking <- function(alpha, K, rng = NULL, seed = 0) {
  a <- as.numeric(alpha)
  n <- as.integer(K)
  if (a <= 0.0) stop("slowdp: alpha must be positive")
  if (n < 1L) stop("slowdp: at least one stick is needed")
  if (!is.null(rng)) {
    e <- rng
  } else {
    e <- .ghc_rng(seed)
  }
  p <- numeric(n)
  Vs <- numeric(n)
  rest <- 1.0
  for (i in seq_len(n)) {
    v <- .slowdp_beta1alpha(e, a)
    Vs[i] <- v
    p[i] <- v * rest
    rest <- rest * (1.0 - v)
  }
  list(weights = as.numeric(p), V = as.numeric(Vs),
       remaining = rest, kept_mass = sum(p), K = n, alpha = a,
       note = "the remaining stick is the mass truncation throws away")
}

#' truncation_error
#'
#' A step of the slowdp_native implementation. Called by \code{decay_diagnostics}, \code{sticks_for_tolerance}, \code{truncated_dp}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param alpha Coerced to numeric by the body, with \code{as.numeric}.
#' @param K Coerced to integer by the body, with \code{as.integer}.
#' @return A list with \code{expected_tail}, \code{kept}, \code{alpha}, \code{K}, \code{per_stick_factor}, \code{note}.
#' @export
truncation_error <- function(alpha, K) {
  a <- as.numeric(alpha)
  n <- as.integer(K)
  if (a <= 0.0 || n < 1L)
    stop("slowdp: need alpha > 0 and K >= 1")
  e <- (a / (1.0 + a)) ^ n
  list(expected_tail = e, kept = 1.0 - e, alpha = a, K = n,
       per_stick_factor = a / (1.0 + a),
       note = "a more diffuse process needs more sticks for the same fidelity")
}

#' sticks_for_tolerance
#'
#' A step of the slowdp_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param alpha Coerced to numeric by the body, with \code{as.numeric}.
#' @param tol Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.001}.
#' @return A list with \code{K}, \code{expected_tail}, \code{tolerance}, \code{note}.
#' @export
sticks_for_tolerance <- function(alpha, tol = 1e-3) {
  a <- as.numeric(alpha)
  t <- as.numeric(tol)
  if (a <= 0.0) stop("slowdp: alpha must be positive")
  if (!(t > 0.0 && t < 1.0))
    stop("slowdp: the tolerance must lie in (0,1)")
  f <- a / (1.0 + a)
  K <- as.integer(ceiling(log(t) / log(f)))
  K <- max(1L, K)
  list(K = K,
       expected_tail = truncation_error(a, K)$expected_tail,
       tolerance = t,
       note = "chosen from the closed form, not guessed")
}

#' decay_diagnostics
#'
#' A step of the slowdp_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param weights Coerced to numeric by the body, with \code{as.numeric}.
#' @param alpha Passed to \code{truncation_error}.
#' @return A list with \code{realised_tail}, \code{expected_tail}, \code{ratio}, \code{largest_index}, \code{monotone}, \code{note}.
#' @export
decay_diagnostics <- function(weights, alpha) {
  p <- as.numeric(weights)
  K <- length(p)
  if (K < 1L) stop("slowdp: no weights given")
  exp_tail <- truncation_error(alpha, K)$expected_tail
  realised <- max(0.0, 1.0 - sum(p))
  biggest_late <- which.max(p)
  monotone <- TRUE
  if (K >= 2L) {
    for (i in seq_len(K - 1L)) {
      if (p[i] + 1e-12 < p[i + 1]) { monotone <- FALSE; break }
    }
  }
  list(realised_tail = realised, expected_tail = exp_tail,
       ratio = if (exp_tail > 1e-12) realised / exp_tail else Inf,
       largest_index = as.integer(biggest_late),
       monotone = monotone,
       note = paste0("the sticks are NOT ordered; a late large stick ",
                     "is exactly what a mean-based truncation misses"))
}

#' truncated_dp
#'
#' A step of the slowdp_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param alpha Coerced to numeric by the body, with \code{as.numeric}.
#' @param K Coerced to integer by the body, with \code{as.integer}.
#' @param base_sampler Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @param rng Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @param seed Passed to \code{.ghc_rng}. Defaults to \code{0}.
#' @param renormalise A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return A list with \code{estimate}, \code{weights}, \code{atoms}, \code{discarded_mass}, \code{expected_discarded}, \code{renormalised}, \code{K}, \code{alpha}, \code{method}, \code{note}.
#' @export
truncated_dp <- function(alpha, K, base_sampler = NULL, rng = NULL,
                         seed = 0, renormalise = TRUE) {
  if (!is.null(rng)) e <- rng else e <- .ghc_rng(seed)
  sb <- stick_breaking(alpha, K, rng = e)
  p <- as.numeric(sb$weights)
  tail <- sb$remaining
  if (isTRUE(renormalise)) {
    z <- sum(p)
    if (z <= 1e-12)
      stop("slowdp: the kept sticks carry no mass")
    p <- p / z
  }
  atoms <- if (!is.null(base_sampler)) {
    vapply(seq_len(as.integer(K)), function(i) as.numeric(base_sampler(e)),
           numeric(1))
  } else {
    seq_len(as.integer(K)) - 1L
  }
  list(estimate = as.numeric(p), weights = as.numeric(p),
       atoms = as.numeric(atoms), discarded_mass = tail,
       expected_discarded =
         truncation_error(alpha, K)$expected_tail,
       renormalised = isTRUE(renormalise), K = as.integer(K),
       alpha = as.numeric(alpha),
       method = "truncated stick-breaking; Sethuraman (1994)",
       note = paste0("renormalising moves the discarded mass onto the ",
                     "survivors, which is why the amount is returned"))
}

#' .slowdp_cheatsheet
#'
#' A step of the slowdp_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.slowdp_cheatsheet <- function() {
  paste0("slowdp: Sethuraman writes the DP as a PROGRAM -- ",
         "p_k = V_k prod(1 - V_l) with V_k ~ Beta(1, alpha) and ",
         "atoms from the base measure, summing to 1 almost surely. ",
         "Truncating at K leaves an expected tail of EXACTLY ",
         "(alpha/(1+alpha))^K: geometric in K, and worse for larger ",
         "alpha, so a diffuse process needs more sticks. Invert it ",
         "to CHOOSE K rather than guess. But the decay is only in ",
         "EXPECTATION and the sticks are NOT ordered -- a single ",
         "draw can put a large stick late, which is what ",
         "'slow-decreasing' names. Renormalising the survivors ",
         "silently absorbs the discarded mass, so report it.")
}

# ledger/NAMING.md compact alias
dp_truncation <- truncated_dp
slow_dp_truncate <- truncated_dp
slowdptruncate <- truncated_dp

morie_slowdp <- list(stick_breaking = stick_breaking,
                     truncation_error = truncation_error,
                     sticks_for_tolerance = sticks_for_tolerance,
                     decay_diagnostics = decay_diagnostics,
                     truncated_dp = truncated_dp,
                     cheatsheet = .slowdp_cheatsheet,
                     dp_truncation = dp_truncation,
                     slow_dp_truncate = slow_dp_truncate,
                     slowdptruncate = slowdptruncate)
