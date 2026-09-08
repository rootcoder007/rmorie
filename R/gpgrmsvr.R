# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Genomic relationship matrices, normalization and support vector
# machinery -- R mirror of the Python modules vanr1, vanr2, yangr,
# zscnm, unitl, varrd, svmhp, svmsl, svmep.
#
# Sources consulted, not recalled:
#   Montesinos Lopez, Montesinos Lopez & Crossa (2022), Multivariate
#   Statistical Machine Learning Methods for Genomic Prediction,
#   Springer, DOI 10.1007/978-3-030-89010-0.  Sec. 2.4 pp.50-52 (the
#   three GRM methods, with the printed G matrices used as test
#   values), sec. 2.6 pp.57-58 (normalization), eqs. (9.6)-(9.8)
#   pp.344-346, (9.34)-(9.37) pp.354-355, (9.44)-(9.45) p.357,
#   sec. 15.4.1 pp.641-642 (splitting rules).
#   VanRaden, P.M. (2008). J Dairy Sci 91:4414-4423.
#   Yang, J. et al. (2010). Nat Genet 42:565-569, and GCTA.
#   Smola, A.J. & Scholkopf, B. (2004). A tutorial on support vector
#   regression. Statistics and Computing 14:199-222, eqs. (4), (10),
#   (11), (16) -- the book explicitly does NOT derive SVR (p.369).
#
# Svmhp and Svmsl delegate to the existing core mirrors Hardsvm and
# Softsvm rather than solving the dual a second time.  Everything else
# is closed form, so this arm reproduces the Python arm to machine
# precision; the SVR dual runs a fixed 4000 projected-gradient steps
# with no tolerance-driven early exit, for the same reason.
#
# Collision scan: gpgrmsvr.R and all nine exported names were free in
# both R trees and in _lazy_map.json at the time of writing.

#' Minor allele frequencies for markers coded 0/1/2: MVSML p.51 uses
#'
#' phat = colMeans(X)/2.
#'
#' @param M A matrix; passed to \code{as.matrix}.
#' @param freq Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @return A numeric value.
#' @export
#' @examples
#' X <- cbind(1, c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9), c(0.4, 1.1, 0.9, 1.8, 2.2,
#' 2.6, 3.4, 3.9))
#' res <- .s02freq(M = X)
#' res
.s02freq <- function(M, freq = NULL) {
  # Minor allele frequencies for markers coded 0/1/2: MVSML p.51 uses
  # phat = colMeans(X)/2.
  if (!is.null(freq)) return(as.numeric(freq))
  as.numeric(colMeans(as.matrix(M))) / 2
}

#' VanRaden method 1 genomic relationship matrix
#'
#' \deqn{G = ZZ^T / (2 \sum_j p_j(1-p_j))}{G = ZZ' / (2 sum_j p_j(1-p_j))}
#' with \eqn{Z_{ij} = M_{ij} - 2p_j}.  VanRaden (2008) method 1; the
#' same matrix is MVSML (2022) method 2, p.51, whose printed example is
#' reproduced exactly by this function.
#'
#' @param marker_matrix Lines by markers, coded 0, 1, 2.
#' @param freq Optional allele frequencies; column means over 2.
#' @return Named list with `estimate`, `G`, `freq`, `denominator`,
#'   `n_lines`, `n_markers`, `method`.
#' @references VanRaden (2008) J Dairy Sci 91:4414-4423.
#' @examples
#' Vanr1(matrix(c(1, 0, 0, 1, 2, 1), nrow = 2))
#' @export
Vanr1 <- function(marker_matrix, freq = NULL) {
  M <- as.matrix(marker_matrix)
  storage.mode(M) <- "double"
  pj <- .s02freq(M, freq)
  Z <- sweep(M, 2, 2 * pj, "-")
  den <- 2 * sum(pj * (1 - pj))
  G <- tcrossprod(Z) / den
  list(estimate = mean(diag(G)), G = G, freq = pj,
       denominator = den, n_lines = nrow(M), n_markers = ncol(M),
       method = "VanRaden (2008) method 1 genomic relationship matrix")
}

#' VanRaden method 2 genomic relationship matrix (marker weighted)
#'
#' \deqn{G = \sum_j w_j z_j z_j^T / (2 \sum_j w_j p_j(1-p_j))}{G = sum_j w_j z_j z_j' /
#' (2 sum_j w_j p_j(1-p_j))}
#' The default weights are VanRaden's reciprocal marker variances,
#' \eqn{w_j = 1/(2p_j(1-p_j))}, which give rare markers more influence.
#' Unit weights reproduce method 1 exactly.
#'
#' @param marker_matrix Lines by markers, coded 0, 1, 2.
#' @param weights Optional per-marker weights.
#' @param freq Optional allele frequencies.
#' @return Named list with `estimate`, `G`, `freq`, `weights`,
#'   `denominator`, `n_lines`, `n_markers`, `method`.
#' @references VanRaden (2008) J Dairy Sci 91:4414-4423.
#' @examples
#' Vanr2(matrix(c(1, 0, 0, 1, 2, 1), nrow = 2))
#' @export
Vanr2 <- function(marker_matrix, weights = NULL, freq = NULL) {
  M <- as.matrix(marker_matrix)
  storage.mode(M) <- "double"
  pj <- .s02freq(M, freq)
  vr <- 2 * pj * (1 - pj)
  w <- if (is.null(weights)) ifelse(vr > 0, 1 / ifelse(vr > 0, vr, 1), 0)
       else as.numeric(weights)
  Z <- sweep(M, 2, 2 * pj, "-")
  den <- sum(w * vr)
  G <- (Z * rep(w, each = nrow(M))) %*% t(Z) / den
  list(estimate = mean(diag(G)), G = G, freq = pj, weights = w,
       denominator = den, n_lines = nrow(M), n_markers = ncol(M),
       method = "VanRaden (2008) method 2 weighted relationship matrix")
}

#' Yang et al. realized genomic relationship matrix
#'
#' \deqn{A_{jk} = N^{-1} \sum_i (x_{ij}-2p_i)(x_{ik}-2p_i)/(2p_i(1-p_i))}{A_jk = (1/N)
#' sum_i (x_ij-2p_i)(x_ik-2p_i)/(2p_i(1-p_i))}
#' Each locus is standardized by its own Hardy-Weinberg variance.  The
#' original paper gives the diagonal as
#' \eqn{1 + N^{-1}\sum_i \[x_{ij}^2 - (1+2p_i)x_{ij} + 2p_i^2\]/(2p_i(1-p_i))};
#' GCTA later made the diagonal match the off-diagonal, which is the
#' default here.
#'
#' @param marker_matrix Lines by markers, coded 0, 1, 2.
#' @param freq Optional allele frequencies.
#' @param yang_diagonal Use the Yang et al. (2010) diagonal.
#' @return Named list with `estimate`, `A`, `freq`, `n_lines`,
#'   `n_markers`, `yang_diagonal`, `method`.
#' @references Yang et al. (2010) Nat Genet 42:565-569; GCTA.
#' @examples
#' Yangr(matrix(c(1, 0, 0, 1, 2, 1), nrow = 2))
#' @export
Yangr <- function(marker_matrix, freq = NULL, yang_diagonal = FALSE) {
  M <- as.matrix(marker_matrix)
  storage.mode(M) <- "double"
  J <- nrow(M)
  p <- ncol(M)
  pi_ <- if (!is.null(freq)) as.numeric(freq) else colMeans(M) / 2
  vr <- 2 * pi_ * (1 - pi_)
  keep <- vr > 0
  Z <- sweep(M, 2, 2 * pi_, "-")
  Zs <- Z[, keep, drop = FALSE] / rep(sqrt(vr[keep]), each = J)
  A <- tcrossprod(Zs) / p
  if (isTRUE(yang_diagonal)) {
    Mk <- M[, keep, drop = FALSE]
    pk <- pi_[keep]
    vk <- vr[keep]
    num <- Mk^2 - sweep(Mk, 2, 1 + 2 * pk, "*") + rep(2 * pk^2, each = J)
    diag(A) <- 1 + rowSums(sweep(num, 2, vk, "/")) / p
  }
  list(estimate = mean(diag(A)), A = A, freq = pi_, n_lines = J,
       n_markers = p, yang_diagonal = isTRUE(yang_diagonal),
       method = "Yang et al. (2010) realized relationship matrix")
}

#' Z-score (standardization) normalization
#'
#' \deqn{X_i^* = (X_i - \mu)/\sigma}{X* = (X - mu)/sigma}
#' MVSML (2022) sec. 2.6 p.57.  `ddof = 1` is the sample standard
#' deviation, matching the `scale()` the book's own examples call.
#'
#' @param x Numeric vector to standardize.
#' @param ddof Delta degrees of freedom of the standard deviation.
#' @return Named list with `estimate`, `x_std`, `mean`, `sd`, `n`,
#'   `method`.
#' @references MVSML (2022) sec. 2.6 p.57.
#' @examples
#' Zscnm(c(1, 2, 3, 4))
#' @export
Zscnm <- function(x, ddof = 1) {
  v <- as.numeric(x)
  n <- length(v)
  mu <- sum(v) / n
  den <- n - ddof
  sdv <- if (den > 0) sqrt(sum((v - mu)^2) / den) else 0
  out <- if (sdv > 0) (v - mu) / sdv else rep(0, n)
  list(estimate = sdv, x_std = out, mean = mu, sd = sdv, n = n,
       method = "z-score standardization (MVSML 2022 p.57)")
}

#' Unit-length (L2) normalization
#'
#' \deqn{x/\|x\|_2}{x / ||x||_2}
#' Classical; deliberately carries no book citation.  MVSML (2022)
#' sec. 2.6 pp.57-58 lists five normalizations and this is not one of
#' them.  A zero vector has no direction and is returned unchanged.
#'
#' @param x Numeric vector.
#' @return Named list with `estimate`, `x_unit`, `norm`, `n`, `method`.
#' @references Classical; no single owning source.
#' @examples
#' Unitl(c(3, 4))
#' @export
Unitl <- function(x) {
  v <- as.numeric(x)
  nrm <- sqrt(sum(v * v))
  out <- if (nrm > 0) v / nrm else v
  list(estimate = nrm, x_unit = out, norm = nrm, n = length(v),
       method = "unit-length (L2) normalization")
}

#' Variance reduction criterion for regression tree splitting
#'
#' \deqn{\Delta = Var(t) - (n_L/n)Var(t_L) - (n_R/n)Var(t_R)}{Delta = Var(t) -
#' (nL/n)Var(tL) - (nR/n)Var(tR)}
#' with population variances -- the CART impurity decrease.  MVSML
#' (2022) sec. 15.4.1 p.642 prints the same criterion in its weighted
#' sum-of-squares form, \eqn{SSE_L\Omega_L + SSE_R\Omega_R}, which is
#' returned alongside: the two rank splits identically but are not the
#' same number.
#'
#' @param y Responses at the node.
#' @param split_idx Logical mask or integer positions of the left child.
#' @return Named list with `estimate`, `delta_var`, `sse_weighted`,
#'   `sse_left`, `sse_right`, `var_parent`, `var_left`, `var_right`,
#'   `n_left`, `n_right`, `omega_left`, `omega_right`, `method`.
#' @references MVSML (2022) sec. 15.4.1 pp.641-642; Breiman et al.
#'   (1984) ch.8.4.
#' @examples
#' Varrd(c(1, 2, 3, 10, 11, 12), c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
#' @export
Varrd <- function(y, split_idx) {
  v <- as.numeric(y)
  n <- length(v)
  left_mask <- if (is.logical(split_idx) && length(split_idx) == n) {
    split_idx
  } else {
    seq_len(n) %in% as.integer(split_idx)
  }
  left <- v[left_mask]
  right <- v[!left_mask]
  pvar <- function(z) if (length(z) == 0) 0 else sum((z - mean(z))^2) / length(z)
  sse <- function(z) if (length(z) == 0) 0 else sum((z - mean(z))^2)
  nl <- length(left)
  nr <- length(right)
  wl <- nl / n
  wr <- nr / n
  vp <- pvar(v)
  vl <- pvar(left)
  vr <- pvar(right)
  dv <- vp - wl * vl - wr * vr
  list(estimate = dv, delta_var = dv,
       sse_weighted = sse(left) * wl + sse(right) * wr,
       sse_left = sse(left), sse_right = sse(right),
       var_parent = vp, var_left = vl, var_right = vr,
       n_left = nl, n_right = nr, omega_left = wl, omega_right = wr,
       method = "variance reduction split (MVSML 2022 sec. 15.4.1)")
}

#' Maximum margin (hard margin) hyperplane
#'
#' MVSML (2022) eqs. (9.6)-(9.8) pp.344-346, solved through the Wolfe
#' dual (9.32)-(9.33).  Delegates to the shared core mirror rather than
#' solving the dual a second time.
#'
#' @param X Inputs, n by p.
#' @param y Labels in \{-1, +1\}.
#' @param ... Passed to the dual solver.
#' @return Named list; `estimate` is the margin \eqn{M = 1/\|\beta\|}.
#' @references MVSML (2022) eqs. (9.6)-(9.8) pp.344-346.
#' @examples
#' Svmhp(matrix(c(1, 2, 4, 5, 1, 2, 4, 3), ncol = 2), c(-1, -1, 1, 1))
#' @export
Svmhp <- function(X, y, ...) {
  f <- Hardsvm(X, y, ...)
  f$estimate <- f$margin
  f$method <- "SVM maximum margin hyperplane (MVSML 2022 eqs. 9.6-9.8)"
  f
}

#' Support vector classifier with slack variables (soft margin)
#'
#' MVSML (2022) eqs. (9.34)-(9.37) pp.354-355, dual (9.44)-(9.45)
#' p.357.  `C` is the box bound of (9.45); the realized slack total is
#' reported as `slack_sum`.  Delegates to the shared core mirror.
#'
#' @param X Inputs, n by p.
#' @param y Labels in \{-1, +1\}.
#' @param C Box bound on the multipliers.
#' @param ... Passed to the dual solver.
#' @return Named list; `estimate` is the margin.
#' @references MVSML (2022) eqs. (9.34)-(9.37), (9.44)-(9.45).
#' @examples
#' Svmsl(matrix(c(1, 2, 4, 5, 1, 2, 4, 3), ncol = 2), c(-1, -1, 1, 1), 1)
#' @export
Svmsl <- function(X, y, C, ...) {
  f <- Softsvm(X, y, C, ...)
  f$estimate <- f$margin
  f$method <- "SVM soft margin (MVSML 2022 eqs. 9.34-9.37)"
  f
}

#' Epsilon-insensitive support vector regression
#'
#' MVSML (2022) sec. 9.6 p.369 introduces SVR but states that detailed
#' SVR theory is not covered, referring to Burges (1998).  The
#' equations here are therefore Smola & Scholkopf (2004): the
#' \eqn{\epsilon}-insensitive loss (4), the dual (10), the support
#' vector expansion (11) and the offset interval (16).  Fixed
#' iteration count, no tolerance-driven early exit.
#'
#' @param X Inputs, n by p.
#' @param y Continuous responses.
#' @param C Trade-off constant of the primal.
#' @param eps Half-width of the insensitive tube.
#' @param n_iter Projected-gradient steps.
#' @param kernel,gamma,degree,coef0 Passed to the shared Gram builder.
#' @return Named list with `estimate` (mean epsilon-insensitive loss),
#'   `w`, `b`, `alpha`, `alpha_star`, `theta`, `support_vectors`,
#'   `fitted`, `loss`, `objective`, `method`.
#' @references Smola & Scholkopf (2004) Stat Comput 14:199-222.
#' @examples
#' Svmep(matrix(1:5, ncol = 1), c(1.2, 1.9, 3.2, 3.9, 5.1), 1, 0.1)
#' @export
Svmep <- function(X, y, C, eps, n_iter = 4000L, kernel = "linear",
                  gamma = NULL, degree = 2, coef0 = 1) {
  Xm <- as.matrix(X)
  storage.mode(Xm) <- "double"
  ys <- as.numeric(y)
  n <- length(ys)
  Cv <- as.numeric(C)
  ev <- as.numeric(eps)
  K <- morie_kernel_matrix(Xm, kernel = kernel, gamma = gamma,
                           degree = degree, coef0 = coef0)
  K <- as.matrix(K)
  scale_ <- max(abs(diag(K)))
  if (!(scale_ > 0)) scale_ <- 1
  step <- 1 / (n * scale_)
  a <- rep(0, n)
  b <- rep(0, n)
  for (it in seq_len(as.integer(n_iter))) {
    th <- a - b
    Kt <- as.numeric(K %*% th)
    ga <- -Kt - ev + ys
    gb <- Kt - ev - ys
    sh <- (sum(ga) - sum(gb)) / (2 * n)
    ga <- ga - sh
    gb <- gb + sh
    a <- pmin(Cv, pmax(0, a + step * ga))
    b <- pmin(Cv, pmax(0, b + step * gb))
  }
  th <- a - b
  w <- as.numeric(crossprod(Xm, th))
  Kt <- as.numeric(K %*% th)
  lo <- (-ev + ys - Kt)[a < Cv - 1e-12 | b > 1e-12]
  hi <- (ev + ys - Kt)[a > 1e-12 | b < Cv - 1e-12]
  b0 <- 0.5 * ((if (length(lo)) max(lo) else 0) +
                 (if (length(hi)) min(hi) else 0))
  fit <- Kt + b0
  loss <- pmax(0, abs(ys - fit) - ev)
  obj <- -0.5 * sum(th * Kt) - ev * sum(a + b) + sum(ys * th)
  list(estimate = sum(loss) / n, w = w, b = b0, alpha = a,
       alpha_star = b, theta = th,
       support_vectors = which(abs(th) > 1e-9) - 1L,
       fitted = fit, loss = loss, objective = obj,
       method = "epsilon-insensitive SVR (Smola & Scholkopf 2004 eq. 10)")
}
