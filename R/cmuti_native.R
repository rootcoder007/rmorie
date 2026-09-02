# Copula-based mutual information.
# Source: Calsaverini & Vicente (2009), EPL 88, 68003
# (fetched-wave3/calsaverini-vicente-2009-copula-information-epl88.pdf):
# MI is the marginal-invariant dependence component;
# I_Gauss(rho) = -log(1 - rho^2)/2.  Linfoot (1957), Information and
# Control 1, 85-89: r_1 = sqrt(1 - exp(-2 I)).  Mirrors Python
# morie.fn.cmuti exactly (average ranks on ties, rank/(n+1)
# transform, normal scores).

#' Mutual information via the Gaussian copula
#'
#' Empirical copula transform rank/(n+1) with average ranks on ties,
#' normal scores z = qnorm(u), then the Gaussian-copula closed form
#' I = -log(1 - rho^2)/2 at the normal-scores correlation.  Being
#' rank-based, the estimate is exactly invariant under strictly
#' increasing marginal transformations.  Also returns Linfoot's
#' informational coefficient of correlation sqrt(1 - exp(-2 I)).
#'
#' @param x,y Paired numeric vectors (n >= 3).
#' @return A list with elements \code{estimate} (MI in nats),
#'   \code{rho}, \code{linfoot_r}, \code{n}, \code{method}.
#' @references Calsaverini, R. S. and Vicente, R. (2009). An
#'   information-theoretic approach to statistical dependence: Copula
#'   information. EPL, 88, 68003.  Linfoot, E. H. (1957). An
#'   informational measure of correlation. Information and Control,
#'   1, 85-89.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_cmuti(V, V)
morie_cmuti <- function(x, y) {
  xv <- as.numeric(x)
  yv <- as.numeric(y)
  n <- length(xv)
  if (length(yv) != n || n < 3) stop("x and y must be paired with n >= 3")
  u <- rank(xv, ties.method = "average") / (n + 1)
  v <- rank(yv, ties.method = "average") / (n + 1)
  zx <- qnorm(u)
  zy <- qnorm(v)
  sxx <- sum((zx - mean(zx))^2)
  syy <- sum((zy - mean(zy))^2)
  if (sxx <= 0 || syy <= 0) stop("degenerate sample (a variable is constant)")
  rho <- sum((zx - mean(zx)) * (zy - mean(zy))) / sqrt(sxx * syy)
  rho <- max(min(rho, 1 - 1e-12), -1 + 1e-12)
  mi <- -0.5 * log(1 - rho^2)
  list(estimate = mi,
       rho = rho,
       linfoot_r = sqrt(1 - exp(-2 * mi)),
       n = n,
       method = paste0("Gaussian-copula MI, I = -log(1-rho^2)/2 ",
                       "(Calsaverini & Vicente 2009; Linfoot 1957)"))
}
