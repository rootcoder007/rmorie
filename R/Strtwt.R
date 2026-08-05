# SPDX-License-Identifier: AGPL-3.0-or-later
#' Stabilized inverse-probability-of-treatment weights within strata
#'
#' The unstabilized weight 1/f(A|H,S) is unbounded: a unit whose observed
#' treatment was nearly impossible given its history carries nearly
#' infinite weight.  Stabilization divides it by the probability of the
#' same treatment under a model that drops the history, leaving the
#' weighted pseudo-population unchanged but shrinking the weights towards
#' one.  Keeping the stratum variables in the numerator makes the weights
#' valid for a marginal structural model that conditions on the stratum.
#' Both models are logistic regressions fitted by IRLS.
#'
#' Formula: sw_i = f(A_i | S_i) / f(A_i | H_i, S_i).
#'
#' @param A Binary treatment coded 0/1.
#' @param H Optional history covariates, n by p.  If NULL the two models
#'   coincide and every weight is exactly one.
#' @param S Optional stratum covariates, n by q, entering both models.
#' @return List with \code{estimate} (mean weight), \code{weights},
#'   \code{unstabilized}, \code{num}, \code{den}, \code{max}, \code{min},
#'   \code{sd}, \code{n}, \code{method}.
#' @references Cole, S. R. and Hernan, M. A. (2008). Constructing inverse
#'   probability weights for marginal structural models. American Journal
#'   of Epidemiology 168(6):656-664. \doi{10.1093/aje/kwn164}
#' @examples
#' Strtwt(c(1, 0, 1, 1, 0, 1, 0, 0), NULL, matrix(c(0, 0, 1, 1, 0, 1, 1, 0), 8, 1))
#' @export
Strtwt <- function(A, H = NULL, S = NULL) {
  a <- .s03vec(A); n <- length(a)
  if (n == 0L) stop("stratified_weights: A is empty")
  if (any(a != 0 & a != 1)) stop("stratified_weights: A must be coded 0/1")
  Sm <- .strtwt_cols(S, n, "S")
  Hm <- .strtwt_cols(H, n, "H")
  Zn <- cbind(1, Sm)
  Zd <- cbind(1, Sm, Hm)
  pn <- .strtwt_fit(Zn, a)
  pd <- .strtwt_fit(Zd, a)
  num <- ifelse(a > 0.5, pn, 1 - pn)
  den <- ifelse(a > 0.5, pd, 1 - pd)
  sw <- num / den
  m <- sum(sw) / n
  v <- if (n > 1) sum((sw - m)^2) / (n - 1) else 0
  list(estimate = as.numeric(m), weights = sw, unstabilized = 1 / den,
       num = num, den = den, max = as.numeric(max(sw)),
       min = as.numeric(min(sw)), sd = as.numeric(sqrt(v)),
       n = as.integer(n),
       method = "sw = f(A|S) / f(A|H,S), stabilized IPTW [Cole & Hernan 2008]")
}

.strtwt_cols <- function(X, n, nm) {
  if (is.null(X)) return(matrix(numeric(0), n, 0))
  M <- .s03mat(X)
  if (nrow(M) != n) stop(sprintf("stratified_weights: %s has the wrong number of rows", nm))
  M
}

.strtwt_fit <- function(Z, a) {
  b <- .s03logit(Z, a, 60L)
  p <- vapply(.s03matvec(Z, b), .s03sigmoid, 0)
  pmin(pmax(p, 1e-12), 1 - 1e-12)
}

# CANONICAL TEST
# r <- Strtwt(c(1, 0, 1, 1, 0, 1, 0, 0), NULL,
#             matrix(c(0, 0, 1, 1, 0, 1, 1, 0), 8, 1))
# stopifnot(all(abs(r$weights - 1) < 1e-9))
