# SPDX-License-Identifier: AGPL-3.0-or-later

#' Dirichlet-multinomial conjugate model
#'
#' Formula: p ~ Dir(alpha); y ~ Multinomial(n, p)
#'
#' The posterior is Dir(alpha + y), so the posterior mean of p_j is
#' (y_j + alpha_j) / (n + sum alpha) and the marginal likelihood is the
#' Polya distribution
#' n! Gamma(A)/Gamma(n+A) prod Gamma(y_j+a_j)/(y_j! Gamma(a_j)).  With
#' two categories this is exactly the beta-binomial.
#'
#' @param counts Observed category counts, non-negative.
#' @param alpha Dirichlet prior; a scalar is recycled.
#' @return List with \code{estimate}, \code{post_mean}, \code{post_var},
#'   \code{post_alpha}, \code{log_marginal}, \code{n}, \code{K},
#'   \code{method}.
#' @references Gelman et al. (2013), Bayesian Data Analysis, 3rd ed.,
#'   CRC, ch. 3.
#' @export
Diripr <- function(counts, alpha = 1) {
  y <- .s03vec(counts)
  K <- length(y)
  if (K == 0L) stop("empty input: counts has no categories")
  if (any(y < 0)) stop("counts must be non-negative")
  a <- .s03vec(alpha)
  if (length(a) == 1L) a <- rep(a, K)
  if (length(a) != K)
    stop("alpha must be scalar or one value per category")
  if (any(a <= 0)) stop("alpha must be strictly positive")
  n <- sum(y)
  A <- sum(a)
  post <- a + y
  P <- A + n
  mean <- post / P
  var <- post * (P - post) / (P * P * (P + 1))
  lm <- lgamma(n + 1) + lgamma(A) - lgamma(n + A)
  for (j in seq_len(K))
    lm <- lm + lgamma(y[j] + a[j]) - lgamma(y[j] + 1) - lgamma(a[j])
  .t1_result(estimate = mean[1], post_mean = mean, post_var = var,
             post_alpha = post, log_marginal = lm, n = n, K = K,
             method = "Dirichlet-multinomial conjugate model")
}
