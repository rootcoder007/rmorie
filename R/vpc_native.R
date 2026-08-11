# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Variance partition coefficient for two-level models (Vpc).
# Bit-identical mirror of src/morie/fn/vpc.py. Printed anchor:
# Goldstein-Browne-Rasbash voting example, sigma2_u0 = 0.142 gives
# Method D VPC = 0.041.

#' Variance partition coefficient for a two-level model
#'
#' For the continuous-response variance-components model the VPC is
#' \eqn{\sigma^2_u / (\sigma^2_u + \sigma^2_e)}. For a binary response
#' with a logit link, the latent-variable approach (Method D of
#' Goldstein, Browne and Rasbash 2002) takes the level-1 residual to be
#' standard logistic with variance \eqn{\pi^2/3 = 3.29}, so
#' \eqn{VPC = \sigma^2_u / (\sigma^2_u + \pi^2/3)}; for the probit link
#' the standard normal latent residual has variance 1.
#'
#' @param sigma2_u Level-2 (between-cluster) variance.
#' @param sigma2_e Level-1 residual variance (identity link only).
#' @param link One of \code{"identity"}, \code{"logistic"},
#'   \code{"probit"}.
#' @return List with \code{estimate}, \code{sigma2_u}, \code{sigma2_1},
#'   \code{link}, \code{method}.
#' @references Goldstein, H., Browne, W. and Rasbash, J. (2002),
#'   Partitioning variation in multilevel models, Understanding
#'   Statistics 1(4), 223-231; preprint sec. 2 eq. 1 and sec. 3.5
#'   (Method D), eqs. 5-6 (source library/pdf/fetched-wave3/
#'   goldstein-browne-rasbash-2002-partitioning-variation.pdf).
#' @export
Vpc <- function(sigma2_u, sigma2_e = NULL, link = "logistic") {
  s2u <- as.numeric(sigma2_u)
  if (s2u < 0) stop("sigma2_u must be nonnegative", call. = FALSE)
  link <- tolower(as.character(link))
  if (link %in% c("identity", "gaussian", "normal", "continuous")) {
    if (is.null(sigma2_e)) stop("sigma2_e required for identity link", call. = FALSE)
    s21 <- as.numeric(sigma2_e)
    if (s21 < 0) stop("sigma2_e must be nonnegative", call. = FALSE)
  } else if (link %in% c("logistic", "logit")) {
    s21 <- pi * pi / 3
  } else if (link == "probit") {
    s21 <- 1
  } else {
    stop("link must be identity, logistic or probit", call. = FALSE)
  }
  est <- if ((s2u + s21) > 0) s2u / (s2u + s21) else NaN
  list(
    estimate = est,
    sigma2_u = s2u,
    sigma2_1 = s21,
    link = link,
    method = "Variance partition coefficient (latent-variable Method D for binary links)"
  )
}
