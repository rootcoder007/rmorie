# SPDX-License-Identifier: AGPL-3.0-or-later
#' Controlled direct effect (Robins-Greenland)
#'
#' \deqn{CDE(m) = (\theta_1 + \theta_3 m)(a - a^*).}{CDE(m) = (theta1 + theta3 m)(a - a*).}
#'
#' Robins, J. M. and Greenland, S. (1992), "Identifiability and exchangeability
#' for direct and indirect effects", \emph{Epidemiology} 3(2), 143-155,
#' doi:10.1097/00001648-199203000-00013, is the shelf citation and where the
#' controlled and natural effects are defined; it is closed access with no open
#' copy in any repository (Unpaywall reports is_oa false, oa_locations empty).
#' The regression-based identification used here was read instead from an open
#' source that states it in closed form, Valeri, L. and VanderWeele, T. J.
#' (2013), "Mediation analysis allowing for exposure-mediator interactions and
#' causal interpretation", \emph{Psychological Methods} 18(2), 137-150,
#' doi:10.1037/a0031034, open access at PMC3659198, equation (0.3), with a the
#' new exposure level and a* the baseline one:
#'
#' \preformatted{CDE = (theta1 + theta3 m)(a - a*)
#' NDE = {theta1 + theta3 (beta0 + beta1 a* + beta2' c)}(a - a*)
#' NIE = (theta2 beta1 + theta3 beta1 a)(a - a*)}
#'
#' from the mediator model E[M|a,c] = beta0 + beta1 a + beta2' c and the
#' outcome model E[Y|a,m,c] = theta0 + theta1 a + theta2 m + theta3 a m +
#' theta4' c, both fitted here by ordinary least squares.  Valeri and
#' VanderWeele's NDE is the pure one and their NIE the total one; the mirror
#' images TNDE and PNIE follow by swapping a and a* and are returned as well.
#'
#' The causal reading of these numbers needs the identification assumptions of
#' that paper.  This function does the arithmetic; it cannot check them.
#'
#' @param X Exposure.
#' @param M Mediator.
#' @param Y Outcome.
#' @param m Level at which the mediator is controlled.
#' @param C Optional matrix of covariates, one row per observation.
#' @param a,astar Exposure contrast; the default is 1 versus 0.
#' @return list: estimate (CDE(m)), pnde, tnde, tnie, pnie, te,
#'   mediated_interaction, beta, theta, m, a, astar, n, method.
#' @keywords internal
#' @examples
#' A <- c(0, 0, 0, 0, 1, 1, 1, 1); e <- c(1, -1, 1, -1, 1, -1, 1, -1)
#' M <- 0.4 + 1.5 * A + e; Y <- 1 + 2 * A + 3 * M + 0.5 * A * M
#' Ctde(A, M, Y, 2)$estimate
#' @export
Ctde <- function(X, M, Y, m, C = NULL, a = 1, astar = 0) {
  f <- .med_fit(X, M, Y, C, "controlled_direct_effect")
  eff <- .med_effects(f$beta, f$theta, f$cbar, as.numeric(a), as.numeric(astar))
  mm <- as.numeric(m)
  c(eff, list(estimate = (f$theta[2L] + f$theta[4L] * mm) *
                (as.numeric(a) - as.numeric(astar)),
              m = mm, a = as.numeric(a), astar = as.numeric(astar), n = f$n,
              method = "Controlled direct effect (Robins-Greenland)"))
}

# The two regressions of the regression-based mediation formulas.
#' The two regressions of the regression-based mediation formulas
#'
#' A step of the ctde implementation. Called by \code{Ctde}, \code{Pnie}, \code{Tnie}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param X Passed to \code{.s03vec}.
#' @param M Passed to \code{.s03vec}.
#' @param Y Passed to \code{.s03vec}.
#' @param C Optional; may be \code{NULL}. Passed to \code{.s03mat}.
#' @param who Passed to \code{paste0}.
#' @return A list with \code{beta}, \code{theta}, \code{cbar}, \code{n}.
#' @export
.med_fit <- function(X, M, Y, C, who) {
  a <- .s03vec(X); m <- .s03vec(M); y <- .s03vec(Y)
  n <- length(a)
  if (n == 0L) stop(paste0(who, ": X is empty"))
  if (length(m) != n || length(y) != n) {
    stop(paste0(who, ": X, M and Y must have the same length"))
  }
  if (is.null(C)) {
    cols <- matrix(0, n, 0L)
  } else {
    cols <- .s03mat(C)
    if (nrow(cols) != n) stop(paste0(who, ": C must have one row per observation"))
  }
  k <- ncol(cols)
  if (n < 4L + k) stop(paste0(who, ": too few observations to fit both models"))
  dm <- cbind(1, a, cols)
  dy <- cbind(1, a, m, a * m, cols)
  beta <- .s03lstsq(dm, m)
  theta <- .s03lstsq(dy, y)
  cbar <- if (k > 0L) colSums(cols) / n else numeric(0)
  list(beta = as.numeric(beta), theta = as.numeric(theta), cbar = cbar, n = n)
}

# Valeri and VanderWeele (2013), eq. (0.3), and its two mirror images.
#' Valeri and VanderWeele (2013), eq. (0.3), and its two mirror images
#'
#' A step of the ctde implementation. Called by \code{Ctde}, \code{Pnie}, \code{Tnie}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param beta A vector; its length is taken and its elements indexed.
#' @param theta A vector; indexed elementwise.
#' @param cbar Numeric; combined arithmetically in the body.
#' @param a Numeric; combined arithmetically in the body.
#' @param astar Numeric; combined arithmetically in the body.
#' @return A list with \code{pnde}, \code{tnde}, \code{tnie}, \code{pnie}, \code{te}, \code{mediated_interaction}, \code{beta}, \code{theta}.
#' @export
.med_effects <- function(beta, theta, cbar, a, astar) {
  d <- a - astar
  b0 <- beta[1L]; b1 <- beta[2L]
  bc <- if (length(beta) > 2L) sum(beta[3:length(beta)] * cbar) else 0
  t1 <- theta[2L]; t2 <- theta[3L]; t3 <- theta[4L]
  pnde <- (t1 + t3 * (b0 + b1 * astar + bc)) * d
  tnde <- (t1 + t3 * (b0 + b1 * a + bc)) * d
  tnie <- (t2 * b1 + t3 * b1 * a) * d
  pnie <- (t2 * b1 + t3 * b1 * astar) * d
  list(pnde = pnde, tnde = tnde, tnie = tnie, pnie = pnie, te = pnde + tnie,
       mediated_interaction = t3 * b1 * d * d, beta = beta, theta = theta)
}
