# morie.fn -- function file (rootcoder007/morie)
# (R translation of the Python tlhoest module.)
# Higher-order targeted loss-based estimation.
#
# References
# ----------
# van der Laan, M. J. & Rose, S. (2018) *Targeted Learning in Data
# Science*, Springer, doi:10.1007/978-3-319-65304-4. Chap. 26 (Carone,
# Diaz & van der Laan): generalising the TMLE framework to use
# higher-order rather than first-order asymptotic representations, to
# provide guidelines for estimators with sound finite-sample behaviour
# that are asymptotically efficient under less restrictive conditions;
# the requirement that the second-order remainder tend to zero faster
# than n^{-1/2} for the first-order representation to be useful; and the
# example that when the density of the data-generating distribution is
# directly involved in the target parameter, a density estimator
# converging faster than n^{-1/4} in a suitable norm is often required
# to make that remainder negligible.
#
# Carone, M., Diaz, I. & van der Laan, M. J. (2018) "Higher-Order
# Targeted Loss-Based Estimation", in *Targeted Learning in Data
# Science*, Springer, 483-510, doi:10.1007/978-3-319-65304-4_26.
#
# Robins, J., Li, L., Tchetgen Tchetgen, E. & van der Vaart, A. (2008)
# "Higher order influence functions and minimax estimation of nonlinear
# functionals", in *Probability and Statistics: Essays in Honor of
# David A. Freedman*, IMS, 335-421, doi:10.1214/193940307000000527.
# Higher-order influence functions.

.tlhoest_EPS <- 1e-12

#' .tlhoest_first_order_expansion
#'
#' Part of the tlhoest_native implementation; see the file header for
#' the source it follows.
#'
#' @param D1 See Usage.
#' @param psi_plugin See Usage.
#' @return A list with \code{estimate}, \code{mean_D1}, \code{order}, \code{note}.
#' @export
.tlhoest_first_order_expansion <- function(D1, psi_plugin) {
  d <- as.numeric(D1)
  n <- length(d)
  if (n < 2L) {
    stop("tlhoest: at least 2 observations are needed")
  }
  m <- sum(d) / n
  list(
    estimate = as.numeric(psi_plugin) + m,
    mean_D1 = m,
    order = 1L,
    note = "valid only if the SECOND-order remainder is o(n^{-1/2})"
  )
}

#' .tlhoest_second_order_term
#'
#' Part of the tlhoest_native implementation; see the file header for
#' the source it follows.
#'
#' @param D2_kernel See Usage.
#' @param O See Usage.
#' @param exclude_diagonal Defaults to \code{TRUE}.
#' @return A list with \code{value}, \code{n_pairs}, \code{cost}, \code{note}.
#' @export
.tlhoest_second_order_term <- function(D2_kernel, O, exclude_diagonal = TRUE) {
  obs <- as.list(O)
  n <- length(obs)
  if (n < 2L) {
    stop("tlhoest: a second-order term needs at least 2 observations")
  }
  tot <- 0.0
  m <- 0L
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (isTRUE(exclude_diagonal) && i == j) {
        next
      }
      tot <- tot + as.numeric(D2_kernel(obs[[i]], obs[[j]]))
      m <- m + 1L
    }
  }
  list(
    value = tot / m,
    n_pairs = m,
    cost = "O(n^2)",
    note = "a U-statistic over PAIRS, which is where the curvature the first-order term misses lives"
  )
}

#' morie_tlhoest
#'
#' Part of the tlhoest_native implementation; see the file header for
#' the source it follows.
#'
#' @param psi_plugin See Usage.
#' @param D1 See Usage.
#' @param D2_kernel See Usage.
#' @param O See Usage.
#' @return A list with \code{estimate}, \code{psi}, \code{first_order}, \code{second_order_correction}, \code{n_pairs}, \code{method}, \code{note}.
#' @export
morie_tlhoest <- function(psi_plugin, D1, D2_kernel, O) {
  fo <- .tlhoest_first_order_expansion(D1, psi_plugin)
  so <- .tlhoest_second_order_term(D2_kernel, O)
  list(
    estimate = fo$estimate + so$value,
    psi = fo$estimate + so$value,
    first_order = fo$estimate,
    second_order_correction = so$value,
    n_pairs = so$n_pairs,
    method = "higher-order targeted loss-based estimation; van der Laan & Rose (2018) Chap. 26",
    note = "the remainder that must be o(n^{-1/2}) is now THIRD order"
  )
}

morie_tlhoest_first_order_expansion <- .tlhoest_first_order_expansion

morie_tlhoest_second_order_term <- .tlhoest_second_order_term

#' morie_tlhoest_rate_requirement
#'
#' Part of the tlhoest_native implementation; see the file header for
#' the source it follows.
#'
#' @param order See Usage.
#' @param n Defaults to \code{1000L}.
#' @return A list with \code{order}, \code{required_rate_per_nuisance}, \code{example_n}, \code{error_at_that_rate}, \code{note}.
#' @export
morie_tlhoest_rate_requirement <- function(order, n = 1000L) {
  o <- as.integer(order)
  if (o < 1L) {
    stop("tlhoest: the order must be at least 1")
  }
  per <- 0.5 / (o + 1L)
  list(
    order = o,
    required_rate_per_nuisance = per,
    example_n = as.integer(n),
    error_at_that_rate = as.integer(n)^(-per),
    note = "each additional order relaxes the per-nuisance rate requirement"
  )
}

#' morie_tlhoest_remainder_order
#'
#' Part of the tlhoest_native implementation; see the file header for
#' the source it follows.
#'
#' @param order See Usage.
#' @return A list with \code{expansion_order}, \code{remainder_order}, \code{must_be}, \code{note}.
#' @export
morie_tlhoest_remainder_order <- function(order) {
  o <- as.integer(order)
  list(
    expansion_order = o,
    remainder_order = o + 1L,
    must_be = "o(n^{-1/2})",
    note = "naming the order is naming the assumption doing the work"
  )
}

#' morie_tlhoest_cheatsheet
#'
#' Part of the tlhoest_native implementation; see the file header for
#' the source it follows.
#'
#' @return A character value.
#' @export
morie_tlhoest_cheatsheet <- function() {
  paste0(
    "tlhoest: TMLE's first-order representation works only if ",
    "the SECOND-order remainder is o(n^{-1/2}), which forces ",
    "nuisance rates faster than n^{-1/4} -- unavailable in ",
    "high dimensions or under weak smoothness. Carry the ",
    "expansion further: add a second-order kernel D_2 ",
    "integrated over PAIRS (a U-statistic, O(n^2), diagonal ",
    "excluded), target against both, and now only the THIRD-",
    "order remainder must vanish. Better finite-sample ",
    "behaviour, efficiency under weaker conditions, at ",
    "quadratic cost and needing a second kernel estimated."
  )
}

# compact alias per ledger/NAMING.md
morie_tlhoest_higherordertmle <- morie_tlhoest
