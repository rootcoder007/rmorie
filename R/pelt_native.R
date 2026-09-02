# Exact penalised changepoint detection by PELT.
# Source: Killick, R., Fearnhead, P. and Eckley, I. A. (2012),
# Optimal detection of changepoints with a linear computational cost,
# JASA 107(500), 1590-1598.  Algorithm 2 (the PELT recursion) with the
# pruning rule of their Theorem 3.1, eqs (4)-(5), taken with K = 0 as
# holds for a log-likelihood segment cost.
#
# Native implementation mirroring Python morie.fn.pelt exactly: same
# candidate-set order, same strict "<" tie-breaking (so the EARLIEST
# minimising tau wins, as in the Python loop), same pruning test.
#
# Segment costs (their Sec. 2):
#   "mean"     -- Normal change in mean with unit variance; twice the
#                 negative log-likelihood up to a constant, i.e. the
#                 within-segment sum of squared deviations.
#   "meanvar"  -- Normal change in mean AND variance; twice the
#                 negative log-likelihood n_l (log 2 pi + log
#                 sigma2_hat + 1) using the biased MLE sigma2_hat.

# cumulative sums; .mor_cp_cs(x)$cs[k] is Python cs[k - 1]
#' Cumulative sums; .mor_cp_cs(x)$cs[k] is Python cs[k - 1]
#'
#' A step of the pelt_native implementation. Called by \code{.mor_pelt_core}, \code{morie_binseg}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x A vector; its length is taken and its elements indexed.
#' @return A list with \code{cs}, \code{css}.
#' @export
.mor_cp_tables <- function(x) {
  n <- length(x)
  cs <- numeric(n + 1L)
  css <- numeric(n + 1L)
  for (i in seq_len(n)) {
    cs[i + 1L] <- cs[i] + x[i]
    css[i + 1L] <- css[i] + x[i] * x[i]
  }
  list(cs = cs, css = css)
}

# segment cost of the 0-based half-open block [a, b)
#' Segment cost of the 0-based half-open block [a, b)
#'
#' A step of the pelt_native implementation. Called by \code{.mor_pelt_core}, \code{morie_binseg}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param tab A list; the body reads \code{$cs}, \code{$css} from it.
#' @param cost One of \code{"mean"}, \code{"meanvar"}.
#' @return The value of \code{function}.
#' @export
.mor_cp_cost <- function(tab, cost) {
  log2pi <- log(2 * pi)
  function(a, b) {
    nl <- b - a
    s <- tab$cs[b + 1L] - tab$cs[a + 1L]
    ssdev <- tab$css[b + 1L] - tab$css[a + 1L] - s * s / nl
    if (cost == "mean") return(ssdev)
    if (cost == "meanvar") {
      sig <- ssdev / nl
      if (sig <= 1e-300) sig <- 1e-300
      return(nl * (log2pi + log(sig) + 1))
    }
    stop("cost must be 'mean' or 'meanvar'")
  }
}

#' .mor_pelt_core
#'
#' A step of the pelt_native implementation. Called by \code{morie_pelt}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x A vector; its length is taken.
#' @param cost Passed to \code{.mor_cp_cost}.
#' @param penalty See Usage.
#' @param min_seglen Passed to \code{seq.int}.
#' @return A list with \code{taus}, \code{objective}.
#' @export
.mor_pelt_core <- function(x, cost, penalty, min_seglen) {
  n <- length(x)
  tab <- .mor_cp_tables(x)
  C <- .mor_cp_cost(tab, cost)
  beta <- penalty
  # F and cp are indexed 0..n in Python; here 1..n+1 with an offset
  F <- numeric(n + 1L)
  F[1L] <- -beta
  cp <- integer(n + 1L)
  Rset <- 0L
  K <- 0
  for (t in seq.int(min_seglen, n)) {
    best <- Inf
    barg <- 0L
    for (tau in Rset) {
      if (t - tau < min_seglen) next
      v <- F[tau + 1L] + C(tau, t) + beta
      if (v < best) { best <- v; barg <- tau }
    }
    F[t + 1L] <- best
    cp[t + 1L] <- barg
    keep <- vapply(Rset, function(tau)
      (t - tau < min_seglen) || (F[tau + 1L] + C(tau, t) + K <= F[t + 1L]),
      logical(1))
    Rset <- c(Rset[keep], t)
  }
  taus <- integer(0)
  t <- n
  while (cp[t + 1L] > 0L) {
    taus <- c(taus, cp[t + 1L])
    t <- cp[t + 1L]
  }
  list(taus = rev(taus), objective = F[n + 1L])
}

# segment means of the 0-based bounds vector
#' Segment means of the 0-based bounds vector
#'
#' A step of the pelt_native implementation. Called by \code{morie_binseg}, \code{morie_pelt}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x A vector; indexed elementwise.
#' @param bounds A vector; its length is taken and its elements indexed.
#' @return A vector, from \code{vapply}.
#' @export
.mor_cp_segmeans <- function(x, bounds) {
  vapply(seq_len(length(bounds) - 1L), function(i)
    mean(x[(bounds[i] + 1L):bounds[i + 1L]]), numeric(1))
}

#' Exact penalised changepoint detection (PELT)
#'
#' Minimises \eqn{\sum_{l} C(y_{(\tau_{l-1}+1):\tau_l}) + \beta m}
#' exactly by the pruned dynamic program of Killick, Fearnhead and
#' Eckley (2012), Algorithm 2, which attains the same solution as the
#' full \eqn{O(n^2)} segment-neighbourhood search at close to linear
#' cost.  Pruning uses their Theorem 3.1 with \eqn{K = 0}, valid for a
#' log-likelihood cost.
#'
#' @param x Numeric series.
#' @param cost Segment cost, \code{"mean"} (default; Normal change in
#'   mean, unit variance) or \code{"meanvar"} (Normal change in mean
#'   and variance).  Both routes the paper gives are available.
#' @param penalty Penalty \eqn{\beta} per changepoint.  Default
#'   \code{NULL} uses the BIC/SIC value \code{p * log(n)} with
#'   \code{p = 1} for \code{"mean"} and \code{p = 2} for
#'   \code{"meanvar"} (their Sec. 3.1).
#' @param min_seglen Minimum segment length; forced to at least 2 for
#'   \code{"meanvar"}, where singleton segments make the likelihood
#'   unbounded.
#' @return A list with \code{changepoints} (0-based split positions, as
#'   on the Python side), \code{n_changepoints}, \code{objective},
#'   \code{penalty}, \code{segment_means}, \code{estimate}, \code{n}
#'   and \code{method}.
#' @references Killick, R., Fearnhead, P. and Eckley, I. A. (2012).
#'   Optimal detection of changepoints with a linear computational
#'   cost. Journal of the American Statistical Association, 107(500),
#'   1590-1598.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_pelt(V)
morie_pelt <- function(x, cost = "mean", penalty = NULL, min_seglen = 1L) {
  xs <- as.numeric(x)
  n <- length(xs)
  min_seglen <- as.integer(min_seglen)
  if (cost == "meanvar" && min_seglen < 2L) min_seglen <- 2L
  if (n < 2L * min_seglen) stop("series too short")
  if (is.null(penalty)) {
    p <- if (cost == "mean") 1 else 2
    penalty <- p * log(n)
  }
  penalty <- as.numeric(penalty)
  core <- .mor_pelt_core(xs, cost, penalty, min_seglen)
  taus <- core$taus
  bounds <- c(0L, taus, n)
  list(changepoints = as.numeric(taus),
       n_changepoints = length(taus),
       objective = core$objective,
       penalty = penalty,
       segment_means = .mor_cp_segmeans(xs, bounds),
       estimate = as.numeric(taus),
       n = n,
       method = "PELT (Killick-Fearnhead-Eckley 2012)")
}
