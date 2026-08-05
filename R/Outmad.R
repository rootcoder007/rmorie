#' MAD-median outlier rule
#'
#' Declare an observation an outlier when \code{|x - M| / MADN > crit},
#' where \code{M} is the sample median, \code{MADN = MAD / 0.6745} and
#' \code{MAD} is the unscaled median absolute deviation.  Both \code{M}
#' and \code{MADN} have a breakdown point of 0.5, which is what lets this
#' rule survive the masking that defeats \code{\link{Outms}}.
#'
#' The divisor is Wilcox's 0.6745, not R's \code{stats::mad} constant of
#' 1.4826, so \code{MADN} here equals
#' \code{stats::mad(x, constant = 1 / 0.6745)}; the two agree to about
#' seven significant digits.
#'
#' The constant 2.24 is from Rousseeuw and van Zomeren (1990).  Equation
#' (2.14) is the Hampel identifier, for which Hampel used 3.5 rather than
#' 2.24; \code{crit} is exposed for that reason.
#'
#' @param x Numeric vector; at least two observations.  More than half
#'   the values tied at the median gives \code{MAD = 0} and no usable
#'   scale.
#' @param crit Positive cut-off; defaults to 2.24.  Use 3.5 for Hampel's
#'   original identifier.
#' @return A list with components \code{flag} (0/1 per observation),
#'   \code{which} (1-based positions), \code{out_val}, \code{n_out},
#'   \code{center}, \code{scale}, \code{dis}, \code{crit}, \code{n},
#'   \code{estimate} (= \code{n_out}) and \code{method}.
#' @references
#' Wilcox, R. R. (2017). \emph{Modern Statistics for the Social and
#' Behavioral Sciences: A Practical Introduction}, 2nd edn. CRC Press,
#' section 2.5.2, equation (2.14), p.33.
#'
#' Rousseeuw, P. J. and van Zomeren, B. C. (1990). Unmasking multivariate
#' outliers and leverage points. \emph{Journal of the American
#' Statistical Association} \strong{85}(411), 633-639.
#' @examples
#' x <- c(2, 2, 3, 3, 3, 4, 4, 4, 100000, 100000)
#' Outmad(x)$n_out   # 2 -- both extremes found
#' Outms(x)$n_out    # 0 -- the mean/SD rule is masked
#' @export
Outmad <- function(x, crit = 2.24) {
  x <- as.numeric(x)
  n <- length(x)
  if (n < 2L) stop("Outmad: need at least 2 observations")
  if (anyNA(x)) stop("Outmad: x contains a missing value")
  crit <- as.numeric(crit)
  if (!(crit > 0)) stop("Outmad: crit must be positive")
  center <- stats::median(x)
  mad <- stats::median(abs(x - center))
  scale <- mad / 0.6745
  if (!(scale > 0)) stop("Outmad: the median absolute deviation is zero")
  dis <- abs(x - center) / scale
  flag <- as.integer(dis > crit)
  which_out <- which(flag == 1L)
  list(flag = flag, which = which_out, out_val = x[which_out],
       n_out = length(which_out), center = center, scale = scale,
       dis = dis, crit = crit, n = n,
       estimate = as.numeric(length(which_out)),
       method = "Wilcox (2017) MAD-median outlier rule, eq. (2.14)")
}
