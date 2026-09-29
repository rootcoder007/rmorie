#' Hoeffding confidence bound for a cross-validated risk
#'
#' The cross-validated risk averages \code{n} held-out losses; for losses
#' between 0 and \eqn{B} Hoeffding's inequality gives the half-width
#' \eqn{t = B \sqrt{\log(2/\delta) / (2n)}}. \eqn{B} is the known range of the
#' loss, not the spread of the fold risks. Identical to the Python arm
#' \code{morie.fn.cvbnd.cvbnd}.
#'
#' @param cv_risks Numeric vector of per-fold risks.
#' @param n Total sample size.
#' @param n_folds Number of folds (reported only).
#' @param delta Confidence parameter.
#' @param loss_bound Upper bound of the loss.
#' @return A list with \code{cv_risk}, \code{cv_se}, \code{upper_bound},
#'   \code{lower_bound}, \code{bound_width}, \code{loss_bound}, \code{n} and
#'   \code{delta}.
#' @references Hoeffding, W. (1963). Probability inequalities for sums of
#'   bounded random variables. Journal of the American Statistical Association
#'   58, 13-30.
#' @examples
#' cvbnd(c(0.10, 0.14, 0.12, 0.08, 0.11), n = 500)$bound_width
#' @export
cvbnd <- function(cv_risks, n, n_folds = 5, delta = 0.05, loss_bound = 1) {
  r <- as.numeric(cv_risks)
  k <- length(r)
  if (k == 0) stop("cv_risks must be non-empty.")
  if (!(delta > 0 && delta < 1)) stop("delta must lie in (0, 1).")
  if (!(loss_bound > 0)) stop("loss_bound must be positive.")
  if (n < 1) stop("n must be at least 1.")
  m <- sum(r) / k
  w <- loss_bound * sqrt(log(2 / delta) / (2 * n))
  list(cv_risk = m, cv_se = if (k > 1) sqrt(sum((r - m)^2) / (k - 1) / k) else NaN,
       upper_bound = m + w, lower_bound = m - w, bound_width = w, loss_bound = loss_bound,
       n = n, delta = delta)
}
