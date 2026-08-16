# MVSML eq. (8.13), chapter 9 (support vector machines) eqs. (9.1)-(9.47)
# and chapter 14 (functional regression) eqs. (14.1)-(14.14).
# Mirrors morie.fn._gp_core (Python) function for function.
#
# Montesinos Lopez, Montesinos Lopez & Crossa (2022), Multivariate
# Statistical Machine Learning Methods for Genomic Prediction, Springer
# (DOI 10.1007/978-3-030-89010-0).
#
# These are internal parity mirrors, reached as morie:::Name, exactly as
# the rest of the MVSML mirror is: they exist to check the R arm against
# the Python arm and are not part of the exported API, so they add no
# NAMESPACE entries and need no man/*.Rd.

# --- eq. (8.13) p.296 ------------------------------------------------

#' morie_khatri_rao_rows
#'
#' A step of the gp_mvsml_ch9_14 implementation. Called by \code{Apxkern}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param A A matrix; indexed by row and column.
#' @param B A matrix; indexed by row and column.
#' @return The value of \code{out}, as built in the body.
#' @export
morie_khatri_rao_rows <- function(A, B) {
  A <- as.matrix(A)
  B <- as.matrix(B)
  out <- matrix(0, nrow(A), ncol(A) * ncol(B))
  for (i in seq_len(nrow(A))) {
    out[i, ] <- as.numeric(t(outer(A[i, ], B[i, ])))
  }
  out
}

#' Extended approximate (compressed) kernel model (MVSML eq. 8.13)
#'
#' y = mu 1 + Z_E beta_E + P_u1 f + P_u2 l + eps.  Steps 1-7 of the
#' summary on p.296: P = K_{L,m} U S^(-1/2) is the compressed design of
#' (8.12) built from m of the L lines, P_u1 = Z_u1 P expands it to the n
#' records, and P_u2 = P_u1 : Z_E is the row-wise Kronecker interaction
#' with the environment design.
#' @noRd
Apxkern <- function(X, m_index, Z_u1, Z_E, kernel = "linear",
                    gamma = NULL, tol = 1e-10) {
  sk <- morie_sparse_kernel_design(X, m_index, kernel, gamma, tol)
  Zu1 <- as.matrix(Z_u1)
  ZE <- as.matrix(Z_E)
  Pu1 <- Zu1 %*% sk$P
  Pu2 <- morie_khatri_rao_rows(Pu1, ZE)
  list(
    P = sk$P, P_u1 = Pu1, P_u2 = Pu2,
    design = cbind(1, ZE, Pu1, Pu2),
    widths = c(
      intercept = 1L, environments = ncol(ZE),
      lines = ncol(Pu1), line_x_env = ncol(Pu2)
    ),
    rank = sk$rank
  )
}

# --- eqs. (9.1)-(9.4) pp.339-340 -------------------------------------

#' Hyperplane definition and side (MVSML eqs. 9.1-9.3)
#'
#' beta_0 + beta_1 x_1 + ... + beta_p x_p = 0 defines a (p-1)-dimensional
#' flat subspace (eq. 9.1 for p = 3, eq. 9.2 in general).  A left-hand
#' side < 0 satisfies (9.3) and puts the point on one side, > 0 satisfies
#' (9.4) and puts it on the other.
#' @noRd
Hyperpl <- function(X, beta0, beta) {
  X <- as.matrix(X)
  b <- as.numeric(beta)
  nb <- sqrt(sum(b^2))
  v <- as.numeric(beta0) + as.numeric(X %*% b)
  list(
    value = v, side = sign(v), below = v < 0, above = v > 0,
    on_plane = abs(v) <= 1e-12,
    distance = if (nb > 0) abs(v) / nb else rep(Inf, length(v)),
    norm_beta = nb
  )
}

# --- eqs. (9.6)-(9.8) pp.344-346 -------------------------------------

#' Maximum margin classifier (MVSML eqs. 9.6-9.8)
#'
#' (9.6) maximizes M subject to sum_j beta_j^2 = 1 and
#' y_i(beta_0 + x_i beta) >= M.  Since M = 1 / ||beta|| once the scale is
#' fixed, that is equivalent to minimizing (1/2)||beta||^2 (9.7) subject
#' to y_i(beta_0 + x_i beta) >= 1 (9.8); the street is 2 / ||beta||.
#' @noRd
Hardsvm <- function(X, y, ...) {
  fit <- morie_svm_fit_dual(X, y, C = NULL, ...)
  X <- as.matrix(X)
  ys <- as.numeric(y)
  nb <- sqrt(sum(fit$beta^2))
  f <- fit$beta0 + as.numeric(X %*% fit$beta)
  fm <- ys * f
  list(
    beta = fit$beta, beta0 = fit$beta0, norm_beta = nb,
    margin = if (nb > 0) 1 / nb else Inf,
    street_width = if (nb > 0) 2 / nb else Inf,
    objective = 0.5 * nb^2, functional_margin = fm,
    min_functional_margin = min(fm),
    constraint_ok = min(fm) >= 1 - 1e-6,
    alpha = fit$alpha, support_vectors = fit$support_vectors
  )
}

# --- eqs. (9.9)-(9.14) pp.346-347 ------------------------------------

#' Wolfe dual of a constrained program (MVSML eqs. 9.9-9.14)
#'
#' (9.12) maximizes L = f - sum_i lambda_i h_i - sum_i alpha_i g_i
#' subject to the stationarity condition (9.13),
#' grad f - sum lambda_i grad h_i - sum alpha_i grad g_i = 0, and to
#' alpha_i >= 0 (9.14).  The book warns under (9.14) that the sign of
#' the inequality term is crucial; its own worked examples supply the
#' constraint in the >= form and subtract it, the convention used here.
#' @noRd
Wolfedual <- function(f, grad_f, h = NULL, grad_h = NULL, g = NULL,
                      grad_g = NULL, lam = NULL, alpha = NULL) {
  hv <- if (is.null(h)) numeric(0) else as.numeric(h)
  gv <- if (is.null(g)) numeric(0) else as.numeric(g)
  lm_ <- if (is.null(lam)) rep(0, length(hv)) else as.numeric(lam)
  al <- if (is.null(alpha)) rep(0, length(gv)) else as.numeric(alpha)
  gf <- as.numeric(grad_f)
  Gh <- if (is.null(grad_h)) {
    NULL
  } else {
    matrix(unlist(grad_h),
      nrow = length(hv),
      byrow = TRUE
    )
  }
  Gg <- if (is.null(grad_g)) {
    NULL
  } else {
    matrix(unlist(grad_g),
      nrow = length(gv),
      byrow = TRUE
    )
  }
  L <- as.numeric(f) - sum(lm_ * hv) - sum(al * gv)
  stat <- gf
  if (!is.null(Gh) && length(hv)) stat <- stat - as.numeric(lm_ %*% Gh)
  if (!is.null(Gg) && length(gv)) stat <- stat - as.numeric(al %*% Gg)
  list(
    L = L, stationarity = stat,
    max_stationarity = if (length(stat)) max(abs(stat)) else 0,
    alpha_nonnegative = all(al >= -1e-12),
    n_equality = length(hv), n_inequality = length(gv)
  )
}

# --- eqs. (9.15)-(9.26) pp.346-347 -----------------------------------

#' Quadratic program under one linear inequality (MVSML eqs. 9.15-9.26)
#'
#' minimize z'z subject to a'z >= c.  The Wolfe dual (9.17) is
#' L = z'z - 2 alpha (a'z - c); stationarity (9.18) gives z = alpha a,
#' and substituting back leaves L(alpha) = -(a'a) alpha^2 + 2 c alpha
#' (9.19), maximized at alpha = c / (a'a) >= 0 (9.20).  Illustrative
#' Example 9.1 is a = 1, c = 1; Illustrative Example 9.2 is a = (1, 1),
#' c = 2.  The two are the same problem, so one routine answers both.
#' @noRd
Qplincon <- function(a, c) {
  av <- as.numeric(a)
  aa <- sum(av^2)
  if (aa <= 0) stop("constraint vector a must be nonzero")
  alpha <- as.numeric(c) / aa
  z <- alpha * av
  list(
    x = z, alpha = alpha, dual_quadratic = -aa,
    dual_linear = 2 * as.numeric(c),
    dual_value = -aa * alpha^2 + 2 * as.numeric(c) * alpha,
    primal_value = sum(z^2), constraint = sum(av * z),
    active = TRUE
  )
}

# --- eq. (9.27) p.348 ------------------------------------------------

#' Wolfe primal of the maximum margin problem (MVSML eq. 9.27)
#'
#' L = (1/2)||beta||^2 - sum_i alpha_i [ y_i(beta_0 + x_i beta) - 1 ].
#' Its derivatives with respect to beta and beta_0 are (9.28) and (9.29),
#' both zero at the optimum.
#' @noRd
Svmlagr <- function(X, y, beta0, beta, alpha) {
  X <- as.matrix(X)
  b <- as.numeric(beta)
  ys <- as.numeric(y)
  al <- as.numeric(alpha)
  f <- as.numeric(beta0) + as.numeric(X %*% b)
  slack <- ys * f - 1
  list(
    L = 0.5 * sum(b^2) - sum(al * slack),
    quadratic_term = 0.5 * sum(b^2), slack = slack,
    grad_beta = b - as.numeric(t(X) %*% (al * ys)),
    grad_beta0 = -sum(al * ys)
  )
}

# --- eqs. (9.34)-(9.37) pp.354-355 -----------------------------------

#' Support vector classifier, soft margin (MVSML eqs. 9.34-9.37)
#'
#' (9.34) maximizes M subject to sum_j beta_j^2 = 1 (9.35),
#' y_i(beta_0 + sum_j beta_j x_ij) >= M(1 - zeta_i) (9.36), and
#' zeta_i >= 0 with sum_i zeta_i <= T (9.37).  The book writes T both for
#' that slack budget and for the box bound on the multipliers in (9.45);
#' only (9.45) is directly solvable, so T is the box bound here and the
#' realized sum of slacks is returned as slack_sum.
#' @noRd
Softsvm <- function(X, y, T, ...) {
  fit <- morie_svm_fit_dual(X, y, C = as.numeric(T), ...)
  X <- as.matrix(X)
  ys <- as.numeric(y)
  nb <- sqrt(sum(fit$beta^2))
  f <- fit$beta0 + as.numeric(X %*% fit$beta)
  zeta <- pmax(0, 1 - ys * f)
  list(
    beta = fit$beta, beta0 = fit$beta0, norm_beta = nb,
    margin = if (nb > 0) 1 / nb else Inf,
    zeta = zeta, slack_sum = sum(zeta),
    n_violating = sum(zeta > 1e-9),
    n_misclassified = sum(zeta > 1),
    alpha = fit$alpha, support_vectors = fit$support_vectors,
    objective = fit$objective
  )
}

# --- eqs. (9.38)-(9.43) pp.356-357 -----------------------------------

#' KKT conditions of the support vector classifier (MVSML eqs. 9.38-9.43)
#'
#' (9.38) L = (1/2)||beta||^2 + T sum_i zeta_i
#' - sum_i alpha_i [ y_i(beta_0 + x_i beta) - 1 + zeta_i ]
#' - sum_i delta_i zeta_i, with residuals (9.39) beta - sum alpha_i y_i
#' x_i, (9.40) sum alpha_i y_i, (9.41) alpha_i + delta_i - T, (9.42)
#' alpha_i [ y_i(beta_0 + x_i beta) - 1 + zeta_i ] and (9.43) delta_i
#' zeta_i.
#'
#' The printed sign of the delta term on p.356 is inconsistent with the
#' book's own (9.41), which states dL/dzeta_i = T - alpha_i - delta_i;
#' that requires the term to enter with a minus, and it does so here.
#' @noRd
Svmkkt <- function(X, y, beta0, beta, alpha, delta, zeta, T) {
  X <- as.matrix(X)
  b <- as.numeric(beta)
  ys <- as.numeric(y)
  al <- as.numeric(alpha)
  dl <- as.numeric(delta)
  zt <- as.numeric(zeta)
  Tv <- as.numeric(T)
  f <- as.numeric(beta0) + as.numeric(X %*% b)
  inner <- ys * f - 1 + zt
  L <- 0.5 * sum(b^2) + Tv * sum(zt) - sum(al * inner) - sum(dl * zt)
  r39 <- b - as.numeric(t(X) %*% (al * ys))
  r40 <- sum(al * ys)
  r41 <- al + dl - Tv
  r42 <- al * inner
  r43 <- dl * zt
  worst <- max(abs(c(r39, r40, r41, r42, r43)))
  list(
    L = L, stationarity_beta = r39, balance = r40,
    multiplier_sum = r41, complementary_alpha = r42,
    complementary_delta = r43, max_residual = worst,
    kkt_satisfied = worst < 1e-6
  )
}

# --- eqs. (9.44)-(9.45) p.357 ----------------------------------------

#' Wolfe dual of the support vector classifier (MVSML eqs. 9.44-9.45)
#'
#' maximize L(alpha) = sum_i alpha_i
#' - (1/2) sum_i sum_j alpha_i alpha_j y_i y_j (x_i . x_j) subject to
#' 0 <= alpha_i <= T and sum_i alpha_i y_i = 0.  It differs from the hard
#' margin dual (9.32)-(9.33) only by the upper bound T.
#' @noRd
Svmsdual <- function(X, y, T, K = NULL, ...) {
  Tv <- as.numeric(T)
  fit <- morie_svm_fit_dual(X, y, C = Tv, K = K, ...)
  ys <- as.numeric(y)
  list(
    alpha = fit$alpha, beta = fit$beta, beta0 = fit$beta0,
    objective = fit$objective,
    support_vectors = fit$support_vectors,
    balance = sum(fit$alpha * ys),
    bounded = all(fit$alpha >= -1e-9 & fit$alpha <= Tv + 1e-9),
    at_bound = which(fit$alpha > Tv - 1e-6)
  )
}

# --- eqs. (9.46)-(9.47) p.360 ----------------------------------------

#' Support vector machine with a kernel (MVSML eqs. 9.46-9.47)
#'
#' Because the dual touches the data only through the inner products
#' x_i . x_j, each can be replaced by a positive definite symmetric
#' kernel K(x_i, x_j), which implicitly defines an inner product in an
#' enlarged feature space.  That substitution is the whole difference
#' between (9.44) and (9.46); the constraints (9.47) are unchanged.
#' @noRd
Ksvmdual <- function(X, y, T, kernel = "linear", gamma = NULL,
                     K = NULL, ...) {
  Km <- if (is.null(K)) {
    morie_kernel_matrix(X, kernel, gamma)
  } else {
    as.matrix(K)
  }
  out <- Svmsdual(X, y, T, K = Km, ...)
  out$K <- Km
  out$kernel <- if (is.null(K)) kernel else "precomputed"
  out
}

# --- eq. (14.1) p.579 ------------------------------------------------

#' Functional linear model with scalar response (MVSML eq. 14.1)
#'
#' Y = mu + int_0^T x(t) beta(t) dt + E.  The integral of the product of
#' the centered covariate curve and the coefficient function is taken by
#' the trapezoid rule on the observation grid, the same quadrature the
#' chapter uses for its inner products on p.581.
#' @noRd
Flmint <- function(t, x_values, beta_values, mu = 0) {
  tt <- as.numeric(t)
  xs <- as.numeric(x_values)
  bs <- as.numeric(beta_values)
  m <- length(tt)
  s <- sum(0.5 * diff(tt) * (xs[-m] * bs[-m] + xs[-1] * bs[-1]))
  list(
    integral = s, fitted = as.numeric(mu) + s,
    mu = as.numeric(mu), n_points = m
  )
}

# --- eq. (14.2) p.579 ------------------------------------------------

#' Basis expansion of the coefficient function (MVSML eq. 14.2)
#'
#' beta(t) = sum_{l=1}^{L1} beta_l phi_l(t), the device that makes (14.1)
#' estimable: an infinite-dimensional unknown function is replaced by L1
#' scalars, after which (14.1) collapses to the linear model (14.3).
#' @noRd
Basexp <- function(t, beta_coef, kind = "fourier", period = NULL) {
  coefs <- as.numeric(beta_coef)
  list(
    beta_t = morie_fda_beta_function(t, coefs, length(coefs), kind),
    t = as.numeric(t), n_basis = length(coefs)
  )
}

# --- eq. (14.8) p.581 ------------------------------------------------

#' Basis matrix of the observed curves (MVSML eq. 14.8)
#'
#' Psi is the m x L2 matrix with entry (j, o) equal to psi_o(t_j): rows
#' are the times at which the covariate curve was observed, columns the
#' L2 basis functions.  It is what turns a discretely sampled curve into
#' basis coefficients through (14.7).
#' @noRd
Basmat <- function(t, n_basis, kind = "fourier", period = NULL) {
  Psi <- morie_fda_basis(t, n_basis, kind, period)
  list(
    Psi = Psi, m = nrow(Psi), L2 = ncol(Psi),
    PsiTPsi = t(Psi) %*% Psi
  )
}

# --- eq. (14.11) p.601 -----------------------------------------------

# p-th derivative of the basis functions of (14.2).  For the Fourier
# basis phi_0 = 1, phi_{2k-1} = sin(2 pi k t / P) and
# phi_{2k} = cos(2 pi k t / P), so the p-th derivative is (2 pi k / P)^p
# times a quarter-period phase shift.  Mirrors the Python
# _gp_core.fda_basis_derivative, including its polynomial convention
# u = (t - lo) / span, which differs from morie_fda_basis above.
#' P-th derivative of the basis functions of (14.2).  For the Fourier
#'
#' basis phi_0 = 1, phi_{2k-1} = sin(2 pi k t / P) and phi_{2k} = cos(2
#' pi k t / P), so the p-th derivative is (2 pi k / P)^p times a
#' quarter-period phase shift.  Mirrors the Python
#' _gp_core.fda_basis_derivative, including its polynomial convention u
#' = (t - lo) / span, which differs from morie_fda_basis above.
#'
#' @param t Coerced to numeric by the body, with \code{as.numeric}.
#' @param n_basis Coerced to integer by the body, with \code{as.integer}.
#' @param p A count; the body uses it as \code{seq_len(...)}. Defaults to \code{1L}.
#' @param kind Passed to \code{identical}. Defaults to \code{"fourier"}.
#' @param period Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @return The value of \code{out}, as built in the body.
#' @export
morie_fda_basis_deriv <- function(t, n_basis, p = 1L, kind = "fourier",
                                  period = NULL) {
  tt <- as.numeric(t)
  L <- as.integer(n_basis)
  p <- as.integer(p)
  lo <- min(tt)
  hi <- max(tt)
  span <- hi - lo
  if (span == 0) span <- 1
  P <- if (is.null(period)) span else as.numeric(period)
  out <- matrix(0, length(tt), L)
  for (l in seq_len(L) - 1L) {
    if (identical(kind, "fourier")) {
      if (l == 0L) {
        out[, 1L] <- if (p == 0L) 1 else 0
        next
      }
      k <- if (l %% 2L == 1L) (l + 1L) %/% 2L else l %/% 2L
      w <- 2 * pi * k / P
      phase <- w * tt + 0.5 * pi * p
      out[, l + 1L] <- (w^p) * (if (l %% 2L == 1L) {
        sin(phase)
      } else {
        cos(phase)
      })
    } else {
      if (p > l) {
        out[, l + 1L] <- 0
        next
      }
      cf <- 1
      if (p > 0L) for (j in seq_len(p) - 1L) cf <- cf * (l - j)
      out[, l + 1L] <- cf * (((tt - lo) / span)^(l - p)) / span^p
    }
  }
  out
}

#' Roughness penalty matrix (MVSML eq. 14.11)
#'
#' J_beta = int_0^T [ d^p beta(t) / dt^p ]^2 dt.  Under the basis
#' expansion (14.2) the book writes J_beta = beta' P beta with
#' P_ij = int_0^T phi_i^(p)(t) phi_j^(p)(t) dt.  The chapter says p is
#' typically 1 or 2.  Integrals by the trapezoid rule on the grid t.
#' @noRd
Penmat <- function(t, L1, p = 2L, kind = "fourier", period = NULL,
                   beta = NULL) {
  tt <- as.numeric(t)
  L <- as.integer(L1)
  D <- morie_fda_basis_deriv(tt, L, p, kind, period)
  m <- length(tt)
  dt <- diff(tt)
  P <- matrix(0, L, L)
  for (i in seq_len(L)) {
    for (j in i:L) {
      s <- sum(0.5 * dt * (D[-m, i] * D[-m, j] + D[-1, i] * D[-1, j]))
      P[i, j] <- s
      P[j, i] <- s
    }
  }
  out <- list(P = P, order = p, L1 = L)
  if (!is.null(beta)) {
    b <- as.numeric(beta)
    out$J <- as.numeric(t(b) %*% P %*% b)
  }
  out
}

# --- eq. (14.10) p.599 -----------------------------------------------

#' Penalized sum of squared errors (MVSML eq. 14.10)
#'
#' SSE_lambda(beta) = sum_i ( y_i - mu - sum_l x_il beta_l )^2
#' + lambda J_beta, with J_beta the penalty (14.11).  lambda trades fit
#' against smoothness: at lambda = 0 it is least squares, and as lambda
#' grows beta(t) is driven towards a constant.
#' @noRd
Pensse <- function(y, X, beta, lam, P, mu = 0) {
  ys <- as.numeric(y)
  X <- as.matrix(X)
  b <- as.numeric(beta)
  P <- as.matrix(P)
  fitted <- as.numeric(mu) + as.numeric(X %*% b)
  resid <- ys - fitted
  J <- as.numeric(t(b) %*% P %*% b)
  sse <- sum(resid^2)
  list(
    sse = sse, penalty = J, lambda = as.numeric(lam),
    objective = sse + as.numeric(lam) * J,
    fitted = fitted, residuals = resid
  )
}

# --- eq. (14.12) p.601 -----------------------------------------------

#' Penalized functional regression fit (MVSML eq. 14.12)
#'
#' With the spectral decomposition P = Gamma D Gamma', X* = X Gamma and
#' beta* = Gamma' beta, (14.10) becomes
#' SSE_lambda(beta*) = ||y - 1_n mu - X* beta*||^2 + lambda beta*' D
#' beta*, minimized at beta* = (X*'X* + lambda D)^-1 X*'(y - 1_n mu),
#' with beta = Gamma beta*.  Zero eigenvalues of a rank-deficient P
#' contribute nothing, the reduction the book notes.
#' @noRd
Penfreg <- function(y, X, P, lam, mu = NULL, tol = 1e-10) {
  ys <- as.numeric(y)
  X <- as.matrix(X)
  P <- as.matrix(P)
  n <- length(ys)
  L <- ncol(P)
  e <- eigen((P + t(P)) / 2, symmetric = TRUE)
  d <- e$values
  G <- e$vectors
  m <- if (is.null(mu)) mean(ys) else as.numeric(mu)
  Xs <- X %*% G
  A <- t(Xs) %*% Xs + diag(as.numeric(lam) * d, L)
  bstar <- as.numeric(morie_solve(A, as.numeric(t(Xs) %*% (ys - m))))
  beta <- as.numeric(G %*% bstar)
  fitted <- m + as.numeric(X %*% beta)
  resid <- ys - fitted
  sse <- sum(resid^2)
  pen <- sum(as.numeric(lam) * d * bstar^2)
  list(
    beta = beta, beta_star = bstar, Gamma = G, eigenvalues = d,
    X_star = Xs, mu = m, fitted = fitted, residuals = resid,
    sse = sse, penalty = pen, objective = sse + pen,
    rank = sum(d > tol)
  )
}

# --- eqs. (14.13) p.607 and (14.14) p.610 ----------------------------

# The X_EF matrix printed on p.610: the rows of X laid out
# block-diagonally by environment, so record i in environment e
# contributes its functional scores in the columns belonging to e and
# zeros elsewhere.
#
# Written out for all I environments the blocks sum column by column to
# X exactly, so the joint design carrying both X and X_EF is rank
# deficient and beta and beta_EF are not separately identified by least
# squares; the book fits (14.14) in BGLR, where the prior on each block
# resolves that.  The default reference = TRUE drops the first
# environment block, the same reference coding the book applies to the
# environment design itself on p.607, where its code reads
# X_E = model.matrix(~0+Env, data = dat_F)[, -1].  Pass
# reference = FALSE for the redundant parameterization as printed.
#' Written out for all I environments the blocks sum column by column to
#'
#' X exactly, so the joint design carrying both X and X_EF is rank
#' deficient and beta and beta_EF are not separately identified by least
#' squares; the book fits (14.14) in BGLR, where the prior on each block
#' resolves that.  The default reference = TRUE drops the first
#' environment block, the same reference coding the book applies to the
#' environment design itself on p.607, where its code reads X_E =
#' model.matrix(~0+Env, data = dat_F)[, -1].  Pass reference = FALSE for
#' the redundant parameterization as printed.
#'
#' @param X A matrix; indexed by row and column.
#' @param env A vector; indexed elementwise.
#' @param reference A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return A list with \code{X_EF}, \code{levels}, \code{kept_levels}, \code{reference}, \code{n_columns}.
#' @export
morie_fda_env_interaction <- function(X, env, reference = TRUE) {
  X <- as.matrix(X)
  levels_ <- sort(unique(env))
  keep <- if (reference) levels_[-1L] else levels_
  L <- ncol(X)
  out <- matrix(0, nrow(X), length(keep) * L)
  for (i in seq_len(nrow(X))) {
    k <- match(env[i], keep)
    if (!is.na(k)) out[i, (k - 1L) * L + seq_len(L)] <- X[i, ]
  }
  list(
    X_EF = out, levels = levels_, kept_levels = keep,
    reference = reference, n_columns = ncol(out)
  )
}

#' Functional regression with environment effects (MVSML eqs. 14.13-14.14)
#'
#' y = 1_n mu + X_E beta_E + X beta + e (14.13), and with the
#' environment-by-reflectance interaction block,
#' y = 1_n mu + X_E beta_E + X beta + X_EF beta_EF + e (14.14).  X
#' carries the L1 functional scores of (14.4)-(14.5).  Passing X_EF =
#' NULL gives (14.13) and passing it gives (14.14); the two differ by
#' that block alone, which is why one routine covers both.
#' @noRd
Fregenv <- function(y, X, X_E, X_EF = NULL, lam = 0, P = NULL) {
  ys <- as.numeric(y)
  X <- as.matrix(X)
  XE <- as.matrix(X_E)
  n <- length(ys)
  D <- cbind(1, XE, X)
  widths <- c(
    intercept = 1L, environments = ncol(XE),
    functional = ncol(X)
  )
  if (!is.null(X_EF)) {
    XF <- as.matrix(X_EF)
    widths <- c(widths, env_x_functional = ncol(XF))
    D <- cbind(D, XF)
  }
  dimnames(D) <- NULL
  A <- t(D) %*% D
  if (!is.null(P) && as.numeric(lam) != 0) {
    P <- as.matrix(P)
    off <- 1L + ncol(XE)
    idx <- off + seq_len(nrow(P))
    A[idx, idx] <- A[idx, idx] + as.numeric(lam) * P
  }
  coef <- as.numeric(morie_solve(A, as.numeric(t(D) %*% ys)))
  fitted <- as.numeric(D %*% coef)
  resid <- ys - fitted
  off <- 1L
  beta_E <- coef[off + seq_len(widths[["environments"]])]
  off <- off + widths[["environments"]]
  beta <- coef[off + seq_len(widths[["functional"]])]
  off <- off + widths[["functional"]]
  beta_EF <- if (is.null(X_EF)) numeric(0) else coef[-seq_len(off)]
  list(
    coef = coef, mu = coef[1], beta_E = beta_E, beta = beta,
    beta_EF = beta_EF, widths = widths, design = D,
    fitted = fitted, residuals = resid, sse = sum(resid^2),
    n_columns = ncol(D), has_interaction = !is.null(X_EF)
  )
}

#' Functional regression with environment interaction (MVSML eq. 14.14)
#'
#' y = 1_n mu + X_E beta_E + X beta + X_EF beta_EF + e, which adds to
#' (14.13) the environment-by-reflectance interaction.  Pass env, the
#' environment label of each record, to have the block-diagonal X_EF of
#' p.610 built, or pass X_EF directly.
#' @noRd
Fregint <- function(y, X, X_E, X_EF = NULL, env = NULL, lam = 0,
                    P = NULL, reference = TRUE) {
  if (is.null(X_EF) && !is.null(env)) {
    X_EF <- morie_fda_env_interaction(X, env, reference)$X_EF
  }
  out <- Fregenv(y, X, X_E, X_EF = X_EF, lam = lam, P = P)
  out$X_EF <- X_EF
  out
}
