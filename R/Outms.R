#' Mean/standard-deviation outlier rule
#'
#' Declare an observation an outlier when \code{|x - mean(x)| / sd(x) >
#' crit}, with \code{crit = 2} by default.
#'
#' This rule is provided because it is commonly used, not because it is
#' good.  Both the mean and the standard deviation have a breakdown point
#' of \code{1/n}, so outliers inflate the very yardstick meant to find
#' them.  It is the standard demonstration of \emph{masking}: adding a
#' second, larger outlier can stop the first from being detected.  See
#' \code{\link{Outmad}} for a rule that survives this.
#'
#' @param x Numeric vector; at least two observations.
#' @param crit Positive cut-off; defaults to 2.
#' @return A list with components \code{flag} (0/1 per observation),
#'   \code{which} (1-based positions), \code{out_val}, \code{n_out},
#'   \code{center}, \code{scale}, \code{dis}, \code{crit}, \code{n},
#'   \code{estimate} (= \code{n_out}) and \code{method}.
#' @references
#' Wilcox, R. R. (2017). \emph{Modern Statistics for the Social and
#' Behavioral Sciences: A Practical Introduction}, 2nd edn. CRC Press,
#' section 2.5.1, equation (2.13), p.32.
#' @examples
#' x <- c(rep(2, 5), rep(3, 5), rep(4, 5), 1000)
#' Outms(x)$n_out          # 1000 is detected
#' Outms(c(x, 10000))$n_out  # masking: 1000 no longer is
#' @export
Outms <- function(x, crit = 2) {
  x <- as.numeric(x)
  n <- length(x)
  if (n < 2L) stop("Outms: need at least 2 observations")
  if (anyNA(x)) stop("Outms: x contains a missing value")
  crit <- as.numeric(crit)
  if (!(crit > 0)) stop("Outms: crit must be positive")
  center <- sum(x) / n
  scale <- sqrt(sum((x - center)^2) / (n - 1))
  if (!(scale > 0)) stop("Outms: the standard deviation is zero")
  dis <- abs(x - center) / scale
  flag <- as.integer(dis > crit)
  which_out <- which(flag == 1L)
  list(flag = flag, which = which_out, out_val = x[which_out],
       n_out = length(which_out), center = center, scale = scale,
       dis = dis, crit = crit, n = n,
       estimate = as.numeric(length(which_out)),
       method = "Wilcox (2017) mean/SD outlier rule, eq. (2.13)")
}
