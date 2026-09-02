# SPDX-License-Identifier: AGPL-3.0-or-later
#' Semiparametric-efficient treatment effect adjusting for baseline
#'
#' Under randomisation the crude difference in means is already
#' unbiased, so adjustment is about variance, not confounding. Tsiatis
#' augmentation fits each arm separately, which cannot introduce bias
#' under randomisation and keeps the gain when the baseline association
#' differs between arms.
#'
#' Formula: \code{psi = mean[m_1(V) - m_0(V)]} with influence
#' contribution \code{D(y - m1)/pi - (1 - D)(y - m0)/(1 - pi)}.
#'
#' @param y Outcome.
#' @param D Binary randomised treatment.
#' @param X Covariates other than the baseline value.
#' @param baseline Baseline level of the outcome.
#' @return List with \code{estimate}, \code{se}, \code{pi}, \code{shift}, \code{n}.
#' @references Tsiatis, A. A., Davidian, M., Zhang, M. & Lu, X. (2008).
#'   Statistics in Medicine 27:4658-4677, section 3.
#' @export
#' @examples
#' set.seed(1)
#' r <- Scbsft(y = rnorm(10), D = rbinom(10, 1, 0.5), X = rnorm(10), baseline = rnorm(10)); TRUE
Scbsft <- function(y, D, X, baseline) {
  yv <- as.numeric(y); Dv <- as.numeric(D); bl <- as.numeric(baseline)
  n <- length(yv)
  V <- cbind(1, as.matrix(X), bl)
  i1 <- which(Dv > 0.5); i0 <- which(Dv <= 0.5)
  b1 <- .t1_lstsq(V[i1, , drop = FALSE], yv[i1])$beta
  b0 <- .t1_lstsq(V[i0, , drop = FALSE], yv[i0])$beta
  m1 <- as.numeric(V %*% b1); m0 <- as.numeric(V %*% b0)
  pi_ <- length(i1) / n
  psi <- sum(m1 - m0) / n
  ic <- Dv * (yv - m1) / pi_ - (1 - Dv) * (yv - m0) / (1 - pi_) + m1 - m0 - psi
  se <- if (n > 1) sqrt(sum((ic - mean(ic))^2) / (n - 1) / n) else NaN
  shift <- mean(bl[i1]) - mean(bl[i0])
  .t1_result(estimate = psi, se = se, pi = pi_, shift = shift, n = n,
             method = "Tsiatis covariate-adjusted effect with baseline shift")
}
