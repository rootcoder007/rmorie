# IRT moment linking: mean/mean and mean/sigma.
# Source: Weeks (2010), plink, JSS 35(12), Eqs. 7-13
# (fetched-wave3/weeks-2010-plink-JSS35.pdf); Marco (1977) JEM 14
# (mean/sigma); Loyd & Hoover (1980) JEM 17 (mean/mean).  Mirrors
# Python morie.fn.linkmm exactly.

#' IRT moment linking (mean/mean, mean/sigma)
#'
#' Estimates the linear transformation theta_T = A theta_F + B from
#' common-item slope/difficulty parameters: mean/mean uses
#' A = mu(a_F)/mu(a_T); mean/sigma uses A = sd(b_T)/sd(b_F); both use
#' B = mu(b_T) - A mu(b_F).  The transformed from-scale parameters
#' a* = a_F/A, b* = A b_F + B are also returned.
#'
#' @param a_from,b_from Numeric vectors of common-item parameters on
#'   the from scale.
#' @param a_to,b_to The same items' parameters on the to scale.
#' @param method "mean/mean" (default) or "mean/sigma".
#' @return A list with elements \code{A}, \code{B},
#'   \code{a_transformed}, \code{b_transformed}, \code{n_common},
#'   \code{method}.
#' @references Weeks, J. P. (2010). plink: An R package for linking
#'   mixed-format tests using IRT-based methods. Journal of
#'   Statistical Software, 35(12).  Marco, G. L. (1977). Journal of
#'   Educational Measurement, 14, 139-160.  Loyd, B. H. and Hoover,
#'   H. D. (1980). Journal of Educational Measurement, 17, 179-193.
#' @export
#' @examples
#' morie_linkmm(a_from = c(1, 2, 3, 4, 5, 6, 7, 8), b_from = c(1, 2, 3, 4, 5, 6, 7, 8), a_to = c(1, 2, 3, 4, 5, 6, 7, 8), b_to = c(1, 2, 3, 4, 5, 6, 7, 8))
morie_linkmm <- function(a_from, b_from, a_to, b_to,
                         method = "mean/mean") {
  af <- as.numeric(a_from)
  bf <- as.numeric(b_from)
  at <- as.numeric(a_to)
  bt <- as.numeric(b_to)
  s <- length(af)
  if (length(bf) != s || length(at) != s || length(bt) != s || s < 2) {
    stop("need >= 2 common items with matching lengths")
  }
  meth <- tolower(gsub("_", "/", method))
  if (meth == "mean/mean") {
    if (mean(at) == 0) stop("mean of a_to is zero")
    A <- mean(af) / mean(at)
  } else if (meth == "mean/sigma") {
    if (sd(bf) == 0) stop("sd of b_from is zero")
    A <- sd(bt) / sd(bf)
  } else {
    stop("method must be 'mean/mean' or 'mean/sigma'")
  }
  B <- mean(bt) - A * mean(bf)
  list(A = A, B = B,
       a_transformed = af / A,
       b_transformed = A * bf + B,
       n_common = s,
       method = sprintf("IRT moment linking (%s; plink Eqs. 12-13)", meth))
}
