# SPDX-License-Identifier: AGPL-3.0-or-later
#
# S/T/X/R metalearners with OLS base learners (Meta1l). Bit-identical
# mirror of src/morie/fn/meta1l.py.

#' S-, T-, X- and R-metalearners of the CATE with OLS base learners
#'
#' T-learner (Kunzel et al. 2019, Eq. 3): fit mu0 on controls and mu1
#' on treated, tau_T(x) = mu1(x) - mu0(x). S-learner (Eq. 4): one
#' regression with the treatment indicator as a plain feature,
#' tau_S(x) = mu(x, 1) - mu(x, 0), the w coefficient under OLS.
#' X-learner (Eqs. 5-9): impute effects D1 = Y1 - mu0(X1) and
#' D0 = mu1(X0) - Y0, regress each on x, combine
#' tau_X(x) = g(x) tau0(x) + (1 - g(x)) tau1(x) with g the propensity
#' score (their Remark 1), or the constant mean(w) when \code{ps} is
#' missing. R-learner (Nie and Wager 2021, Eq. 4): minimise the R-loss
#' by OLS of the outcome residual on the treatment residual times the
#' design (Robinson residualization). On a saturated linear DGP all
#' four reduce to differences of OLS fits and agree exactly.
#'
#' @param y Outcome, length n.
#' @param w Binary treatment, 0/1.
#' @param X Covariate matrix, n rows.
#' @param ps Optional propensity scores used as the X-learner weight
#'   and the R-learner treatment expectation.
#' @return List with \code{estimate} (list s, t, x, r of mean CATEs),
#'   \code{cate_s}, \code{cate_t}, \code{cate_x}, \code{cate_r},
#'   \code{coef_r}, \code{n}, \code{n_treat}, \code{method}.
#' @references Kunzel, S. R., Sekhon, J. S., Bickel, P. J. and Yu, B.
#'   (2019), Metalearners for estimating heterogeneous treatment
#'   effects using machine learning, PNAS 116(10), 4156-4165,
#'   doi:10.1073/pnas.1804597116, Eqs. 3-9 and Remark 1; local copy
#'   fetched-wave3/kunzel-sekhon-bickel-yu-2019-metalearners-heterogeneous-treatment-effects-PNAS116.pdf.
#'   Nie, X. and Wager, S. (2021), Quasi-oracle estimation of
#'   heterogeneous treatment effects, Biometrika 108(2), 299-319,
#'   doi:10.1093/biomet/asaa076, Eq. 4; local copy
#'   fetched-wave3/nie-wager-2021-quasi-oracle-heterogeneous-treatment-effects-Biometrika108.pdf.
#'   Curth, A. and van der Schaar, M. (2021), Nonparametric estimation
#'   of heterogeneous treatment effects: From theory to learning
#'   algorithms, AISTATS 130, arXiv:2101.10943; local copy
#'   fetched-wave3/curth-vanderschaar-2021-nonparametric-hte-theory-to-learning-AISTATS.pdf.
#' @export
Meta1l <- function(y, w, X, ps = NULL) {
  yv <- as.numeric(y)
  wv <- as.numeric(w)
  Xa <- as.matrix(X)
  storage.mode(Xa) <- "double"
  n <- length(yv)
  if (nrow(Xa) != n || length(wv) != n) {
    stop("y, w, X must have matching first dimension", call. = FALSE)
  }
  if (!all(wv %in% c(0, 1))) stop("w must be binary 0/1", call. = FALSE)
  i1 <- which(wv == 1)
  i0 <- which(wv == 0)
  p <- ncol(Xa)
  if (length(i1) <= p + 1L || length(i0) <= p + 1L) {
    stop("need more than p + 1 units in each arm", call. = FALSE)
  }
  ols <- function(A, b) as.vector(solve(crossprod(A), crossprod(A, b)))
  D <- cbind(1, Xa)

  D1 <- D[i1, , drop = FALSE]
  D0 <- D[i0, , drop = FALSE]
  b1 <- ols(D1, yv[i1])
  b0 <- ols(D0, yv[i0])
  cate_t <- as.vector(D %*% b1) - as.vector(D %*% b0)

  Ds <- cbind(D, wv)
  bs <- ols(Ds, yv)
  cate_s <- rep(bs[p + 2L], n)

  d1 <- yv[i1] - as.vector(D1 %*% b0)
  d0 <- as.vector(D0 %*% b1) - yv[i0]
  t1 <- ols(D1, d1)
  t0 <- ols(D0, d0)
  g <- if (is.null(ps)) rep(mean(wv), n) else as.numeric(ps)
  cate_x <- g * as.vector(D %*% t0) + (1 - g) * as.vector(D %*% t1)

  m_hat <- as.vector(D %*% ols(D, yv))
  e_hat <- if (is.null(ps)) as.vector(D %*% ols(D, wv)) else as.numeric(ps)
  ry <- yv - m_hat
  rw <- wv - e_hat
  Dr <- D * rw
  br <- ols(Dr, ry)
  cate_r <- as.vector(D %*% br)

  list(estimate = list(s = mean(cate_s), t = mean(cate_t),
                       x = mean(cate_x), r = mean(cate_r)),
       cate_s = cate_s, cate_t = cate_t, cate_x = cate_x,
       cate_r = cate_r, coef_r = br, n = n, n_treat = length(i1),
       method = "S/T/X (Kunzel et al. 2019) + R (Nie-Wager 2021) metalearners, OLS base learners")
}
