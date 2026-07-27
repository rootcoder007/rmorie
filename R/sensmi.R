# SPDX-License-Identifier: AGPL-3.0-or-later

# Internal: least squares with an intercept prepended. Returns the
# coefficients and the residuals in one list so callers do not refit.
.sensmi_ols <- function(X, y) {
  D <- cbind(1, X)
  beta <- drop(qr.solve(D, y))
  list(beta = beta, resid = drop(y - D %*% beta))
}

#' Mediation sensitivity to an unobserved confounder
#'
#' Sensitivity analysis for the average causal mediation effect (ACME)
#' under the linear structural equation model. Fits the Baron-Kenny
#' system, equations (11)-(13) of Imai, Keele & Yamamoto (2010):
#'
#' \deqn{Y_i = \alpha_1 + \beta_1 T_i + \epsilon_{i1}}
#' \deqn{M_i = \alpha_2 + \beta_2 T_i + \epsilon_{i2}}
#' \deqn{Y_i = \alpha_3 + \beta_3 T_i + \gamma M_i + \epsilon_{i3}}
#'
#' Under sequential ignorability the ACME is \eqn{\beta_2\gamma} (their
#' Theorem 2). That assumption fails if an unobserved pre-treatment
#' variable confounds the mediator-outcome relation. Their Theorem 4
#' identifies the ACME for any given
#' \eqn{\rho = Corr(\epsilon_{i2}, \epsilon_{i3})}:
#'
#' \deqn{\bar\delta(\rho) = \frac{\beta_2\sigma_1}{\sigma_2}
#'   \left[\tilde\rho - \rho\sqrt{(1-\tilde\rho^2)/(1-\rho^2)}\right]}
#'
#' with \eqn{\sigma_j^2 = Var(\epsilon_{ij})} and
#' \eqn{\tilde\rho = Corr(\epsilon_{i1}, \epsilon_{i2})}. The ACME is
#' zero exactly at \eqn{\rho = \tilde\rho}, which makes \eqn{\tilde\rho}
#' the breakdown point of the finding.
#'
#' Writing the confounder explicitly as
#' \eqn{\epsilon_{ij} = \lambda_j U_i + \epsilon'_{ij}} gives the
#' partial-\eqn{R^2} reading of the same parameter, the share of
#' otherwise unexplained variance that U accounts for. Imai et al.
#' attribute that parameterisation to Imbens (2003). It satisfies
#' \eqn{\rho^2 = R^{2*}_M R^{2*}_Y}, which is how \code{r2_grid} is
#' interpreted here.
#'
#' Mirrors \code{morie.fn.sensMI} on the Python side.
#'
#' @param y Numeric outcome vector.
#' @param treatment Numeric treatment vector, same length as \code{y}.
#' @param mediator Numeric mediator vector, same length as \code{y}.
#' @param r2_grid Numeric vector of values of the product
#'   \eqn{R^{2*}_M R^{2*}_Y}, each in [0, 1). Defaults to 10 points on
#'   [0, 0.81]. The ACME is reported at both signs of \eqn{\rho},
#'   because its sign follows \eqn{sign(\lambda_2\lambda_3)}, which the
#'   data cannot reveal.
#' @return Named list with \code{estimate} (the ACME under sequential
#'   ignorability), \code{rho_breakdown}, \code{r2_grid},
#'   \code{rho_grid}, \code{acme_positive}, \code{acme_negative},
#'   \code{beta2}, \code{gamma}, \code{sigma1}, \code{sigma2}, \code{n},
#'   \code{method}.
#' @references Imai K, Keele L & Yamamoto T (2010). Identification,
#'   inference and sensitivity analysis for causal mediation effects.
#'   \emph{Statistical Science}, 25(1), 51-71.
#'
#'   Imbens GW (2003). Sensitivity to exogeneity assumptions in program
#'   evaluation. \emph{American Economic Review}, 93(2), 126-132.
#' @examples
#' set.seed(11)
#' n <- 200
#' tr <- rbinom(n, 1, 0.5)
#' md <- 1 + 0.8 * tr + rnorm(n)
#' yy <- 2 + 0.3 * tr + 0.5 * md + rnorm(n)
#' morie_mediation_sensitivity(yy, tr, md)$estimate
#' @export
morie_mediation_sensitivity <- function(y, treatment, mediator,
                                        r2_grid = NULL) {
  y <- as.numeric(y)
  tr <- as.numeric(treatment)
  md <- as.numeric(mediator)
  n <- length(y)
  if (length(tr) != n || length(md) != n) {
    stop("y, treatment and mediator must be the same length; got ",
         n, ", ", length(tr), ", ", length(md), ".", call. = FALSE)
  }
  if (n < 3) stop("Need at least 3 observations, got ", n, ".", call. = FALSE)

  f1 <- .sensmi_ols(tr, y)                  # eq (11)
  f2 <- .sensmi_ols(tr, md)                 # eq (12)
  f3 <- .sensmi_ols(cbind(tr, md), y)       # eq (13)
  beta2 <- unname(f2$beta[2])
  gamma <- unname(f3$beta[3])

  # Population (ddof = 0) moments, matching Var(eps) in the theorem.
  sigma1 <- sqrt(mean((f1$resid - mean(f1$resid))^2))
  sigma2 <- sqrt(mean((f2$resid - mean(f2$resid))^2))
  if (sigma2 <= 0) {
    stop("Mediator is perfectly explained by the treatment; rho is undefined.",
         call. = FALSE)
  }
  rho_tilde <- stats::cor(f1$resid, f2$resid)

  if (is.null(r2_grid)) r2_grid <- seq(0, 0.81, length.out = 10)
  r2 <- as.numeric(r2_grid)
  if (any(r2 < 0) || any(r2 >= 1)) {
    stop("r2_grid holds a product of two R^2 values; each entry must lie in [0, 1).",
         call. = FALSE)
  }

  rho <- sqrt(r2)
  scale <- beta2 * sigma1 / sigma2
  acme <- function(rv) {
    scale * (rho_tilde - rv * sqrt((1 - rho_tilde^2) / (1 - rv^2)))
  }

  list(
    estimate = beta2 * gamma,
    rho_breakdown = rho_tilde,
    r2_grid = r2,
    rho_grid = rho,
    acme_positive = acme(rho),
    acme_negative = acme(-rho),
    beta2 = beta2,
    gamma = gamma,
    sigma1 = sigma1,
    sigma2 = sigma2,
    n = n,
    method = "LSEM mediation sensitivity (Imai-Keele-Yamamoto Thm 4)"
  )
}
