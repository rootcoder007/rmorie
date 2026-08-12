# Variance partition coefficient for multilevel binary models.
# Source: Goldstein, Browne & Rasbash (2002), Partitioning variation
# in multilevel models, Understanding Statistics 1(4), 223-231,
# Sec. 3.4 (Method D, latent-variable approach) and Table 1
# (fetched-wave3/goldstein-browne-rasbash-2002-vpc.pdf).  Mirrors
# Python morie.fn.vpc exactly.

#' Variance partition coefficient (latent-variable method)
#'
#' Goldstein-Browne-Rasbash Method D: with the binary response viewed
#' as a thresholded latent variable, the level-1 latent variance is
#' pi^2/3 = 3.29 for the logit link (1 for probit), so
#' VPC = sigma2_u / (sigma2_u + pi^2/3).
#'
#' @param sigma2_u Level-2 (cluster) variance on the latent scale.
#' @param link "logit" (default) or "probit".
#' @return A list with elements \code{estimate}, \code{sigma2_u},
#'   \code{sigma2_e}, \code{link}, \code{method}.
#' @references Goldstein, H., Browne, W. and Rasbash, J. (2002).
#'   Partitioning variation in multilevel models. Understanding
#'   Statistics, 1(4), 223-231.
#' @export
morie_vpc <- function(sigma2_u, link = "logit") {
  s2u <- as.numeric(sigma2_u)
  if (s2u < 0) stop("sigma2_u must be non-negative")
  lk <- tolower(link)
  s2e <- if (lk == "logit") {
    pi^2 / 3
  } else if (lk == "probit") {
    1
  } else {
    stop("link must be 'logit' or 'probit'")
  }
  list(estimate = s2u / (s2u + s2e),
       sigma2_u = s2u,
       sigma2_e = s2e,
       link = lk,
       method = "latent-variable VPC (Goldstein et al. 2002, Method D)")
}
