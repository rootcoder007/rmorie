# Masters partial credit model probabilities.
# Source: Weeks (2010), plink, JSS 35(12), Eq. 4 and Sec. 2.1
# (fetched-wave3/weeks-2010-plink-JSS35.pdf); Masters (1982),
# Psychometrika 47, 149-174; Muraki (1992), APM 16, 159-176.
# Mirrors Python morie.fn.pcm exactly (log-sum-exp stabilized).

#' Partial credit model category probabilities
#'
#' plink Eq. 4 with the first step parameter dropped:
#' P(X = k | theta) proportional to exp(sum_\{v<=k\} D a (theta - b_v)),
#' empty sum = 0 for the lowest category.  a = 1 gives the Masters
#' (1982) partial credit model; one step reduces to the dichotomous
#' 2PL.
#'
#' @param theta Ability value.
#' @param steps Numeric vector of step parameters b_2, ..., b_K.
#' @param a Common discrimination (1 = Masters PCM).
#' @param D Scaling constant (1 or 1.7).
#' @return A list with elements \code{probabilities},
#'   \code{expected_score}, \code{n_categories}, \code{theta},
#'   \code{a}, \code{D}, \code{method}.
#' @references Masters, G. N. (1982). A Rasch model for partial
#'   credit scoring. Psychometrika, 47, 149-174.  Muraki, E. (1992).
#'   Applied Psychological Measurement, 16, 159-176.  Weeks, J. P.
#'   (2010). Journal of Statistical Software, 35(12).
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_pcm(V, V)
morie_pcm <- function(theta, steps, a = 1, D = 1) {
  th <- as.numeric(theta)
  b <- as.numeric(steps)
  a <- as.numeric(a)
  D <- as.numeric(D)
  if (!length(b)) stop("need at least one step parameter")
  exps <- c(0, cumsum(D * a * (th - b)))
  ex <- exp(exps - max(exps))
  p <- ex / sum(ex)
  list(probabilities = p,
       expected_score = sum((seq_along(p) - 1) * p),
       n_categories = length(p),
       theta = th, a = a, D = D,
       method = "partial credit model (Masters 1982; plink Eq. 4)")
}
