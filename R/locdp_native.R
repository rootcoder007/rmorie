# morie.fn -- function file (rootcoder007/morie)
# Local differential privacy via randomized response -- alias of rrand.
#
# The generated stub described the local model of differential privacy
# (each user randomizes before the collector sees anything), citing
# Kasiviswanathan et al. (2011).  The canonical local-DP mechanism -- and
# the one that paper builds on -- is Warner randomized response, which
# already ships as ``rrand.randomized_response`` with the flip
# probability 1/(1 + e^epsilon) that achieves epsilon-local-DP.  This
# module aliases it rather than adding a second implementation.
#
# References
# ----------
# Kasiviswanathan, S. P., Lee, H. K., Nissim, K., Raskhodnikova, S., &
#     Smith, A. (2011). What can we learn privately? *SIAM Journal on
#     Computing*, 40(3), 793-826. (Local model, section 1; randomized
#     response as the basic local protocol.)
# Warner, S. L. (1965). Randomized response: a survey technique for
#     eliminating evasive answer bias. *JASA*, 60(309), 63-69.
# Dwork, C., & Roth, A. (2014). *FnT-TCS*, 9(3-4), section 3.2.
#     Local source: /run/media/rootcoder/WD_BLACK/library/pdf/fetched-wave3/dwork-roth-2014-algorithmic-foundations-differential-privacy.pdf

# Private helper: compute Warner flip probability for a given epsilon.
# p = 1 / (1 + exp(epsilon))  ->  achieves epsilon-local-DP.
.locdp_flip_prob <- function(epsilon) {
  1.0 / (1.0 + exp(epsilon))
}

# morie_locdp: local-DP Warner randomized response.
#
# Implements the canonical epsilon-local-DP mechanism (Warner 1965, as
# analysed in Kasiviswanathan et al. 2011 sec. 1 and Dwork & Roth 2014
# sec. 3.2).  Each true value x_i in {0,1} is independently flipped
# with probability p = 1 / (1 + exp(epsilon)).
#
# Args:
#   x       : numeric/integer/logical vector of binary values (0/1 or
#             TRUE/FALSE).  Logical inputs are coerced via as.numeric().
#   epsilon : non-negative numeric scalar, privacy budget.  Default 1.0.
#   seed    : integer seed for the RNG, or NULL.  Forwarded to .ghc_rng().
#
# Returns:
#   Named list (RichResult payload) with components:
#     y       : numeric vector, randomized responses (same length as x)
#     epsilon : numeric, the privacy parameter used
#     p       : numeric, the flip probability 1 / (1 + exp(epsilon))
#     n       : integer, length of x
#     rng     : the RNG state returned by .ghc_rng / .ghc_unif
#' Returns:
#'
#' Named list (RichResult payload) with components: y : numeric vector,
#' randomized responses (same length as x) epsilon : numeric, the
#' privacy parameter used p : numeric, the flip probability 1 / (1 +
#' exp(epsilon)) n : integer, length of x rng : the RNG state returned
#' by .ghc_rng / .ghc_unif
#'
#' @param x See Usage.
#' @param epsilon Defaults to \code{1}.
#' @param seed Defaults to \code{NULL}.
#' @return A list with \code{y}, \code{epsilon}, \code{p}, \code{n}, \code{rng}.
#' @export
morie_locdp <- function(x, epsilon = 1.0, seed = NULL) {
  if (!is.numeric(epsilon) || length(epsilon) != 1L ||
      is.na(epsilon) || epsilon < 0) {
    stop("'epsilon' must be a single non-negative numeric value")
  }
  if (is.logical(x)) {
    x <- as.numeric(x)
  } else if (!is.numeric(x)) {
    stop("'x' must be a numeric/integer/logical vector")
  }

  n <- length(x)
  p <- .locdp_flip_prob(epsilon)

  if (is.null(seed)) {
    rng <- .ghc_rng(0L)
  } else {
    rng <- .ghc_rng(as.integer(seed))
  }

  u <- .ghc_unif(rng, n)

  # Flip x_i with probability p; otherwise keep x_i.
  y <- ifelse(u < p, 1 - x, x)

  list(
    y       = y,
    epsilon = epsilon,
    p       = p,
    n       = n,
    rng     = rng
  )
}

# Public aliases mirroring the Python module's exported names.
morie_local_dp <- morie_locdp
morie_localdp  <- morie_locdp

# morie_locdp_cheatsheet: short description string.
#' Morie_locdp_cheatsheet: short description string
#'
#' Part of the locdp_native implementation; see the file header for the
#' source it follows.
#'
#' @return A character value.
#' @export
morie_locdp_cheatsheet <- function() {
  "locdp: local DP randomized response (alias of rrand.randomized_response)."
}
