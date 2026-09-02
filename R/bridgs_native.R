# Bridge sampling for a ratio of normalising constants.
# Source: Meng, X.-L. and Wong, W. H. (1996), Simulating ratios of
# normalizing constants via a simple identity: a theoretical
# exploration, Statistica Sinica 6, 831-860.  Their basic identity
# (eq. 2.1) with the OPTIMAL bridge function of their eq. (3.5),
# which cannot be evaluated without the answer itself and is
# therefore applied by the iterative scheme of their Sec. 4:
#
#   r_(t+1) = [ (1/n2) sum_{x ~ q2} l(x) / (s1 l(x) + s2 r_t) ]
#           / [ (1/n1) sum_{x ~ q1}   1  / (s1 l(x) + s2 r_t) ]
#
# with l = q1/q2 and s_i = n_i / (n1 + n2).
#
# Native implementation mirroring Python morie.fn.bridgs exactly,
# including the shared max-shift that keeps l(x) in range and the
# relative convergence test on log r.

#' Bridge sampling estimate of a ratio of normalising constants
#'
#' Estimates \eqn{r = c_1 / c_2} from draws under each unnormalised
#' density using the optimal bridge of Meng and Wong (1996), eq.
#' (3.5), evaluated by their Sec. 4 fixed-point iteration.  The
#' estimator uses BOTH sets of draws, which is what makes it far more
#' stable than a one-sided importance-sampling ratio when the two
#' densities overlap only partially.
#'
#' @param draws1,draws2 Lists of draws from the first and second
#'   density.
#' @param log_q1,log_q2 Functions giving the unnormalised log
#'   densities.
#' @param tol Relative convergence tolerance on \eqn{\log r}.
#' @param max_iter Maximum iterations.
#' @return A list with \code{ratio}, \code{log_ratio},
#'   \code{iterations}, \code{converged}, \code{n1}, \code{n2},
#'   \code{method}.
#' @references Meng, X.-L. and Wong, W. H. (1996). Simulating ratios
#'   of normalizing constants via a simple identity. Statistica
#'   Sinica, 6, 831-860.
#' @export
#' @examples
#' set.seed(1)
#' morie_bridgs(draws1 = as.list(rnorm(100)), draws2 = as.list(rnorm(100, 0.5)),
#'              log_q1 = function(x) -0.5 * x^2,
#'              log_q2 = function(x) -0.5 * (x - 0.5)^2)
morie_bridgs <- function(draws1, draws2, log_q1, log_q2, tol = 1e-12,
                         max_iter = 1000L) {
  x1 <- as.list(draws1); x2 <- as.list(draws2)
  n1 <- length(x1); n2 <- length(x2)
  if (n1 == 0L || n2 == 0L) stop("both draw sets must be non-empty")
  ll1 <- vapply(x1, function(z) log_q1(z) - log_q2(z), numeric(1))
  ll2 <- vapply(x2, function(z) log_q1(z) - log_q2(z), numeric(1))
  shift <- max(max(ll1), max(ll2))
  l1 <- exp(ll1 - shift)
  l2 <- exp(ll2 - shift)
  s1 <- n1 / (n1 + n2)
  s2 <- n2 / (n1 + n2)
  r <- 1
  converged <- FALSE
  it <- 0L
  for (it in seq_len(as.integer(max_iter))) {
    num <- sum(l2 / (s1 * l2 + s2 * r)) / n2
    den <- sum(1 / (s1 * l1 + s2 * r)) / n1
    r_new <- num / den
    if (abs(log(r_new) - log(r)) <= tol * (1 + abs(log(r_new)))) {
      r <- r_new
      converged <- TRUE
      break
    }
    r <- r_new
  }
  log_ratio <- log(r) + shift
  list(ratio = exp(log_ratio), log_ratio = log_ratio,
       iterations = as.integer(it), converged = converged,
       n1 = n1, n2 = n2,
       method = "Meng-Wong iterative optimal bridge (eq. 3.5 / Sec. 4)")
}
