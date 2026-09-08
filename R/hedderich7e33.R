# SPDX-License-Identifier: AGPL-3.0-or-later
#' Anderson-Darling goodness-of-fit statistic (Hedderich eq. 7.33)
#'
#' Source READ FROM THE CORPUS PDF: Hedderich, Sachs and Reynarowych,
#' Applied Statistics: Methods Using R, section 7.2.8, equation (7.33),
#' citing Anderson and Darling (1952) and Stephens (1986b).  The corpus
#' text layer renders (7.33) with two extraction defects, corrected here
#' against the surrounding prose and Anderson and Darling (1952): the
#' trailing "n sigma 2" is spillover from the neighbouring column and is
#' not part of the statistic, and "1 - Y_\{N+1-i\}" is
#' "1 - F(Y_\{N+1-i\})".  What is computed is the standard published form
#' \code{A2 = -N - (1/N) sum_i (2i-1) (log u_i + log(1 - u_{N+1-i}))}
#' with \code{u_i} the sorted probability-integral transforms.
#'
#' The book notes critical values must be derived separately for each
#' distribution model, so no p-value is returned.
#'
#' @param y Numeric sample.
#' @param cdf Function giving the hypothesised distribution function,
#'   applied to one value at a time.  It must be fully specified.
#' @return list: statistic, u, n, method.
#' @examples
#' Adstat(qnorm(seq(0.05, 0.95, length.out = 20)), pnorm)$statistic
#' @export
Adstat <- function(y, cdf) {
  y <- as.numeric(y)
  u <- vapply(y, function(v) as.numeric(cdf(v)), numeric(1))
  list(
    statistic = Adcore(u), u = sort(u), n = length(y),
    method = "Anderson-Darling A^2 (Hedderich eq. 7.33; Anderson and Darling 1952)"
  )
}

#' Anderson-Darling A^2 from probability-integral transforms
#'
#' The bare statistic behind \code{\link{Adstat}}, kept separate so the
#' GEV and GPD goodness-of-fit wrappers share one implementation.  See
#' \code{\link{Adstat}} for the source and the corrected form of
#' Hedderich eq. (7.33).
#'
#' @param u Numeric vector of probability-integral transforms in \[0, 1\].
#' @return Numeric A^2; \code{Inf} if any value is 0 or 1.
#' @examples
#' Adcore(seq(0.05, 0.95, length.out = 20))
#' @export
Adcore <- function(u) {
  u <- as.numeric(u)
  n <- length(u)
  if (n < 2) stop("need at least 2 observations")
  us <- sort(u)
  if (us[1] <= 0 || us[n] >= 1) {
    return(Inf)
  }
  i <- seq_len(n)
  s <- sum((2 * i - 1) * (log(us[i]) + log(1 - us[n - i + 1])))
  -n - s / n
}
