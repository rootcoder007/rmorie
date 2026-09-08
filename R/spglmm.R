# SPDX-License-Identifier: AGPL-3.0-or-later

#' Conditional specification of a spatial GLMM
#'
#' Data are taken to be conditionally dependent on an underlying smooth
#' spatial process. The link relates the CONDITIONAL mean to the covariates
#' and to the latent field, eq (6.73), \eqn{g[\mu(s)] = x(s)'\beta + S(s)},
#' with the conditional variance depending on the mean through eq (6.74),
#' \eqn{Var[Z(s)|S] = \sigma^2 v(\mu(s))}. Spatial dependence is deferred
#' entirely to the latent process: the data are conditionally independent
#' given S.
#'
#' The trap this function makes visible is the one Sec. 6.3.4 spells out. In
#' a linear model the marginal and conditional specifications agree; in a GLM
#' they do not, because
#' \eqn{E[Z(s)] = E_S[g^{-1}(x(s)'\beta + S(s))] \ne g^{-1}(x(s)'\beta)}.
#' Taking expectations does not carry through a nonlinear link. Both are
#' returned, and for the log link, where Example 6.6 gives the correction in
#' closed form, so is the ratio between them -- \eqn{\exp(\sigma_S^2/2)},
#' which grows with the variance of the latent field.
#'
#' @param X Design matrix.
#' @param beta Coefficient vector.
#' @param S Realised latent spatial field, one value per observation.
#' @param sigma2 Dispersion parameter.
#' @param family One of "poisson", "binomial", "gaussian".
#' @param link_kind Link; defaults to the canonical link of `family`.
#' @param correlation Optional correlation of the latent field; with a log
#'   link the Example 6.6 marginal covariance is then returned too.
#' @return A list with `conditional_mean`, `conditional_variance`,
#'   `naive_marginal_mean`, and for the log link `marginal_mean`,
#'   `marginal_variance` and `marginal_ratio`.
#' @references Schabenberger Ch 6, Sec 6.3.4, eqs (6.73)-(6.74), Example 6.6
#' @export
#' @examples
#' spglmm(X = c(1, 2, 3, 4, 5, 6, 7, 8), beta = 0.5, S = c(1, 2, 3, 4, 5, 6, 7, 8))
spglmm <- function(X, beta, S, sigma2 = 1, family = "poisson",
                   link_kind = NULL, correlation = NULL) {
  if (is.null(link_kind)) link_kind <- .schab_canonical_link(family)
  mu <- .schab_conditional_mean(X, beta, S, link_kind)
  out <- list(conditional_mean = mu,
              conditional_variance = .schab_conditional_variance(mu, sigma2,
                                                                 family),
              naive_marginal_mean = .schab_naive_marginal_mean(X, beta,
                                                               link_kind),
              family = family, link = link_kind, sigma2 = as.numeric(sigma2))
  if (identical(link_kind, "log")) {
    s2S <- as.numeric(stats::var(as.numeric(S)) *
                        (length(S) - 1) / length(S))
    mom <- .schab_marginal_moments_lognormal(X, beta, s2S, sigma2,
                                             rho = correlation)
    out$sigma2_S <- s2S
    out$marginal_mean <- mom$mean
    out$marginal_variance <- mom$variance
    out$marginal_ratio <- exp(s2S / 2)
    if (!is.null(correlation)) out$marginal_covariance <- mom$covariance
    out$marginal_note <- sprintf(
      paste("E[Z(s)] is NOT g^-1(x(s)'beta): under the log link the marginal",
            "mean exceeds the naive value by exp(sigma_S^2/2) = %.4f"),
      out$marginal_ratio)
  } else {
    out$marginal_note <- paste(
      "the marginal mean is E_S[g^-1(x'beta + S)], which has no closed form",
      "for this link; g^-1(x'beta) is NOT it")
  }
  out
}
