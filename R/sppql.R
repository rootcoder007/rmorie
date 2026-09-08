# SPDX-License-Identifier: AGPL-3.0-or-later

#' Fit a spatial GLMM by pseudo-likelihood
#'
#' The problem is linearised with a first-order Taylor expansion of the link,
#' giving the pseudo-data of eq (6.78), whose marginal moments make a linear
#' mixed model. Estimates then follow from (6.80)-(6.82) with the latent
#' field predicted by (6.81). Because the pseudo-data covariance depends on
#' the coefficients the whole thing is relinearised and repeated -- the
#' six-step algorithm the text sets out, which this follows exactly.
#'
#' Wolfinger and O'Connell (1993) call this pseudo-likelihood; Breslow and
#' Clayton (1993) reach the same place by a Laplace approximation and call it
#' penalized quasi-likelihood. The text settles it: the two objectives
#' "differ ... only by a constant amount. The two approaches will thus yield
#' the same estimates." With `check_score = TRUE` the PQL score equations are
#' evaluated at the fitted values, so the equivalence is checked on every fit
#' rather than assumed.
#'
#' There is a choice about where the spatial dependence lives. A marginal
#' model puts it in `R` and sets the latent field to zero; a conditional
#' model puts it in `Sigma_S` and takes `R` to be the identity.
#'
#' @param z Observed responses.
#' @param X Design matrix.
#' @param Sigma_S Covariance of the latent spatial field.
#' @param family One of "poisson", "binomial", "gaussian".
#' @param link_kind Link; defaults to the canonical link.
#' @param sigma2 Dispersion parameter.
#' @param R Working correlation of the conditional part; default identity.
#' @param max_iter,tol Outer relinearisation controls.
#' @param check_score Evaluate the PQL score equations at the solution.
#' @return A list with `beta`, `se_beta`, `cov_beta`, `S`, `mu`, `sigma2`,
#'   `reml`, `converged`, and when checked `score_beta_max`, `score_S_max`
#'   and `pql_pl_equivalent`.
#' @references Schabenberger Ch 6, Sec 6.3.5, eqs (6.78)-(6.85)
#' @export
#' @examples
#' sppql(z = 5L, X = 5L, Sigma_S = 5L)
sppql <- function(z, X, Sigma_S, family = "poisson", link_kind = NULL,
                  sigma2 = 1, R = NULL, max_iter = 100L, tol = 1e-8,
                  check_score = TRUE) {
  if (is.null(link_kind)) link_kind <- .schab_canonical_link(family)
  fit <- .schab_fit_pseudo_likelihood(z, X, Sigma_S, family = family,
                                      link_kind = link_kind, sigma2 = sigma2,
                                      R = R, max_iter = max_iter, tol = tol)
  fit$reml <- .schab_reml_objective(X, fit$Sigma_nu, fit$pseudo_data)
  fit$specification <- if (is.null(R)) "conditional" else "marginal"
  if (!isTRUE(fit$converged)) {
    fit$warning <- sprintf(
      paste("the doubly iterative scheme did not converge in %d outer steps;",
            "the estimates are wherever it stopped"), as.integer(max_iter))
  }
  if (isTRUE(check_score)) {
    sc <- .schab_pql_score(z, X, fit$beta, fit$S, Sigma_S, family, link_kind,
                           sigma2 = sigma2, R = R)
    fit$score_beta_max <- max(abs(sc$score_beta))
    fit$score_S_max <- max(abs(sc$score_S))
    fit$pql_pl_equivalent <- fit$score_beta_max < 1e-6 &&
      fit$score_S_max < 1e-6
    if (!isTRUE(fit$pql_pl_equivalent)) {
      fit$score_warning <- paste(
        "the PQL score equations do not vanish at the pseudo-likelihood",
        "solution, so the two are not agreeing here as Sec. 6.3.5.3 says",
        "they should -- treat the fit as unconverged")
    }
  }
  fit
}
