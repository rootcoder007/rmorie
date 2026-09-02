# SPDX-License-Identifier: AGPL-3.0-or-later
#' High-throughput phenotyping functional predictor combining genomic + phenomic info
#'
#' SOURCE, AND A CORRECTION TO THE STUB THIS REPLACES.  The stub docstring cited
#' "Montesinos Lopez Ch 14" for the whole model, including the genomic term g_i.
#' Chapter 14 does not contain a genomic term at all.  It was read in full --
#' volume \[Pages 579-631\] of Montesinos Lopez, Montesinos Lopez and Crossa
#' (2022), Multivariate Statistical Machine Learning Methods for Genomic
#' Prediction, Springer, doi:10.1007/978-3-030-89010-0 -- and Section 14.1
#' pp.579-583 gives ONLY the functional part: a scalar response, one functional
#' covariate, and no random line effect anywhere.  The genomic term therefore
#' comes from a different chapter and is cited as such below.  It is NOT in
#' Chapter 14.
#'
#' FUNCTIONAL PART -- Chapter 14, Section 14.1, volume \[Pages 579-631\],
#' pp.579-583, all equations read from rendered page images:
#'
#'   (14.1) Y = mu + int_0^T x(t) beta(t) dt + E
#'   (14.2) beta(t) = sum_\{l=1\}^\{L1\} beta_l phi_l(t)
#'   (14.3) Y = mu + sum_l beta_l int x(t) phi_l(t) dt = Xstar beta + E
#'   (14.4) betahat = (Xstar' Xstar)^-1 Xstar' y
#'   (14.5) sigma2hat = (1/n) (y - Xstar betahat)' (y - Xstar betahat)
#'   (14.6) x_i(t) = sum_\{o=1\}^\{L2\} c_io psi_o(t)
#'   (14.7) chat_i = (Psi' Psi)^-1 Psi' x_i(t)            <- p.581 and p.583
#'   (14.8) Psi is m-by-L2 with Psi\[j, o\] = psi_o(t_j)
#'   (14.9) Xstar = \[1n X\], X = Xtilde Psi (Psi'Psi)^-1 Q',
#'          Q\[l, o\] = int_0^T phi_l(t) psi_o(t) dt
#'   p.582  BIC = -2 loglik(betahat, sigma2hat; y) + (L1 + 1) log(n)
#'   p.583  CV1(L2) = sum_j (x(t_j) - xhat_j(t_j))^2, xhat_j the
#'          leave-point-j-out representation with L2 bases
#'
#' Equation (14.9) is implemented as X = Chat Q-transpose, which is the same
#' matrix -- the book derives x_i = Q chat_i on p.581 and only then substitutes
#' (14.7) into it to reach the printed form.  Going through Chat avoids forming
#' and re-solving (Psi-transpose Psi) a second time.
#'
#' GENOMIC PART -- Chapter 5, Section 5.3, equation (5.3), volume
#' \[Pages 141-170\], p.148, read from the same book:
#'
#'   (5.3)  Y = 1n mu + Z_L b + e,  b ~ N_J(0, sigma_g^2 G),  R = sigma^2 In
#'
#' with G the genomic relationship matrix of VanRaden, P. M. (2008), Efficient
#' methods to compute genomic predictions, Journal of Dairy Science
#' 91(11):4414-4423, doi:10.3168/jds.2008-0980, which is the source Section 5.3
#' itself names.  G is built by this package's own morie_grm_vanraden (method 1)
#' rather than re-derived here.
#'
#' THE COMBINED MODEL, which is this function's own composition of the two and
#' is not printed as a single equation anywhere in the book:
#'
#'   y = 1n mu + X beta + Z_L g + e, g ~ N(0, sigma_g^2 G), e ~ N(0, sigma^2 I)
#'
#' with X the functional design of (14.9).  It is solved by Henderson mixed
#' model equations at a fixed variance ratio lam = sigma^2 / sigma_g^2, with one
#' observation per line so that Z_L = I_n.
#'
#' ERRATUM, confirmed by rendered page image, p.584.  The Fourier basis of
#' Section 14.2.1 is printed with phi_2 = phi_3, phi_4 = phi_5 and phi_6 =
#' phi_7.  A repeated set is not a basis: (Psi-transpose Psi) would be singular
#' by construction.  Figure 14.1 on p.585 plots the first five elements for
#' P = 4 on (0, 8) and shows FIVE DISTINCT curves, with phi_1 flat at
#' 0.5 = 1/sqrt(4), phi_2 zero at t = 0 (a sine) and phi_3 at 0.7071 = sqrt(2/4)
#' at t = 0 (a cosine).  The alternating reading is the correct one and is what
#' is implemented: phi_1 = 1/sqrt(P), phi_\{2k\} = sqrt(2/P) sin(k w t),
#' phi_\{2k+1\} = sqrt(2/P) cos(k w t), w = 2 pi / P.
#'
#' Determinism: nothing here is stochastic.  There is no sampling, no fold
#' assignment and no initialisation; the LOOCV of p.583 is exhaustive over the m
#' grid points.
#'
#' @param y length-n vector of scalar responses, one per line.
#' @param markers n-by-p genotype matrix coded \{0, 1, 2\}; G is VanRaden method 1
#'   of it.
#' @param W_functional n-by-m matrix of high-throughput phenotyping curves; row
#'   i is w_i(t) sampled on the common equally spaced grid.
#' @param n_basis L1 = L2, the number of Fourier basis functions, 1 <= L <= m.
#' @param lam the variance ratio sigma^2 / sigma_g^2 of the mixed model
#'   equations.
#' @param a,b end points of the observation grid.
#' @param period P of the Fourier basis; the book takes it as the range of
#'   observed t, which is the default, b - a.
#' @param ridge added to the diagonal of G before inversion; G from a finite
#'   marker panel is singular whenever two lines share a haplotype.
#' @return list: estimate, mu, beta_func, beta, g_hat, coefs, X, Q, fitted,
#'   sigma2, bic, cv1, n, method.
#' @keywords internal
#' @examples
#' g <- seq(0, 1, length.out = 9)
#' W <- t(vapply(1:6, function(i) i * sin(2 * pi * g) + cos(i) * cos(2 * pi * g) + i %% 3,
#'               numeric(9)))
#' M <- matrix((outer(1:6, 1:9, function(i, j) (3 * i + 7 * j) %% 3)), 6, 9)
#' Htpfn(c(1, 2, 3, 2, 1, 0), M, W, n_basis = 3)$mu
#' @export
Htpfn <- function(y, markers, W_functional, n_basis = 5, lam = 1, a = 0, b = 1,
                  period = NULL, ridge = 1e-8) {
  yv <- .s03vec(y)
  n <- length(yv)
  if (n == 0L) stop("htp_functional_predictor: y is empty")
  Wm <- .s03mat(W_functional)
  if (nrow(Wm) != n) {
    stop("htp_functional_predictor: y and W_functional disagree on the number of lines")
  }
  m <- ncol(Wm)
  if (m < 2L) stop("htp_functional_predictor: need at least two grid points")
  Mk <- .s03mat(markers)
  if (nrow(Mk) != n) {
    stop("htp_functional_predictor: y and markers disagree on the number of lines")
  }
  L <- as.integer(n_basis)
  if (L < 1L || L > m) {
    stop("htp_functional_predictor: n_basis must lie between 1 and the number of grid points")
  }
  lam <- as.numeric(lam)
  if (!(lam > 0)) stop("htp_functional_predictor: lam must be positive")
  a <- as.numeric(a)
  b <- as.numeric(b)
  if (!(b > a)) stop("htp_functional_predictor: the grid must have positive width")
  P <- if (is.null(period)) (b - a) else as.numeric(period)
  if (!(P > 0)) stop("htp_functional_predictor: period must be positive")

  h <- (b - a) / (m - 1L)
  tg <- a + (seq_len(m) - 1L) * h
  wq <- rep(h, m)
  wq[1L] <- h * 0.5
  wq[m] <- h * 0.5
  Psi <- matrix(0, m, L)
  for (j in seq_len(m)) Psi[j, ] <- .htpfourier(tg[j] - a, L, P)
  PtP <- .s03crossprod(Psi)
  Chat <- matrix(0, n, L)
  for (i in seq_len(n)) {
    rr <- numeric(L)
    for (l in seq_len(L)) rr[l] <- sum(Psi[, l] * Wm[i, ])
    Chat[i, ] <- .s03ridgesolve(PtP, rr, 1e-12)
  }
  Q <- matrix(0, L, L)
  for (l in seq_len(L)) {
    for (o in seq_len(L)) Q[l, o] <- sum(wq * Psi[, l] * Psi[, o])
  }
  Xd <- matrix(0, n, L)
  for (i in seq_len(n)) {
    for (l in seq_len(L)) Xd[i, l] <- sum(Q[l, ] * Chat[i, ])
  }
  Xs <- cbind(rep(1, n), Xd)
  K <- L + 1L

  G <- morie_grm_vanraden(Mk, method = 1)$estimate
  Ginv <- .htpinvspd(G, as.numeric(ridge))

  XtX <- .s03crossprod(Xs)
  C <- matrix(0, K + n, K + n)
  C[seq_len(K), seq_len(K)] <- XtX
  for (i in seq_len(K)) {
    for (j in seq_len(n)) {
      C[i, K + j] <- Xs[j, i]
      C[K + j, i] <- Xs[j, i]
    }
  }
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      C[K + i, K + j] <- lam * Ginv[i, j] + (if (i == j) 1 else 0)
    }
  }
  rhs <- c(vapply(seq_len(K), function(k) sum(Xs[, k] * yv), 0), yv)
  sol <- tryCatch(.s03cholsolve(C, rhs), error = function(e) {
    stop(paste0("htp_functional_predictor: the mixed model equations are not ",
                "positive definite (", conditionMessage(e), "). The genomic ",
                "block is regularised by lam G^-1 but the fixed-effect block ",
                "is not, so this means the functional design Xstar is rank ",
                "deficient -- typically n_basis >= n, or curves that are ",
                "identical across lines."), call. = FALSE)
  })
  # The Cholesky solve returns a ZERO VECTOR, silently and without error, when
  # the coefficient matrix is not positive definite. Check that the solution
  # actually solves the system rather than trusting it converged.
  scale <- max(1, max(abs(rhs)))
  worst <- 0
  for (i in seq_len(K + n)) {
    acc <- 0
    for (j in seq_len(K + n)) acc <- acc + C[i, j] * sol[j]
    worst <- max(worst, abs(acc - rhs[i]))
  }
  if (!(worst <= 1e-6 * scale)) {
    stop(sprintf(paste0("htp_functional_predictor: the mixed model equations ",
                        "did not solve (residual %.3g); the coefficient matrix ",
                        "is not positive definite"), worst), call. = FALSE)
  }

  beta <- sol[seq_len(K)]
  ghat <- sol[K + seq_len(n)]
  mu <- beta[1L]
  beta_func <- numeric(m)
  for (j in seq_len(m)) beta_func[j] <- sum(beta[1L + seq_len(L)] * Psi[j, ])
  fitted <- numeric(n)
  for (i in seq_len(n)) fitted[i] <- sum(Xs[i, ] * beta) + ghat[i]
  sse <- sum((yv - fitted)^2)
  sigma2 <- sse / n
  bic <- if (sigma2 > 0) {
    n * log(2 * pi * sigma2) + n + (L + 1L) * log(n)
  } else -Inf

  cv1 <- 0
  for (i in seq_len(n)) {
    for (j in seq_len(m)) {
      keep <- setdiff(seq_len(m), j)
      rows <- Psi[keep, , drop = FALSE]
      xs_ <- Wm[i, keep]
      A <- .s03crossprod(rows)
      rr <- numeric(L)
      for (l in seq_len(L)) rr[l] <- sum(rows[, l] * xs_)
      cj <- .s03ridgesolve(A, rr, 1e-12)
      pred <- sum(cj * Psi[j, ])
      cv1 <- cv1 + (Wm[i, j] - pred)^2
    }
  }

  list(estimate = mu, mu = mu, beta_func = beta_func, beta = beta,
       g_hat = ghat, coefs = Chat, X = Xd, Q = Q, fitted = fitted,
       sigma2 = sigma2, bic = bic, cv1 = cv1, n = n,
       method = paste0("y = 1n mu + X beta + Z_L g + e; X from eq. (14.9) with ",
                       "the Fourier basis of p.584 (alternating sin/cos; the ",
                       "printed set repeats), g ~ N(0, sigma_g^2 G) from eq. ",
                       "(5.3); Montesinos Lopez et al. (2022) Ch 14 and Ch 5, ",
                       "G by VanRaden (2008)"))
}

# phi_1..phi_L evaluated at s: the alternating basis of p.584.
#' Phi_1..phi_L evaluated at s: the alternating basis of p.584
#'
#' A step of the htpfn implementation. Called by \code{Htpfn}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param s Numeric; combined arithmetically in the body.
#' @param L A count; the body uses it as \code{seq_len(...)}.
#' @param P Numeric; passed to \code{sqrt}.
#' @return The value of \code{[}.
#' @export
.htpfourier <- function(s, L, P) {
  cc <- sqrt(2 / P)
  w <- 2 * pi / P
  out <- 1 / sqrt(P)
  k <- 1L
  while (length(out) < L) {
    out <- c(out, cc * sin(k * w * s))
    if (length(out) < L) out <- c(out, cc * cos(k * w * s))
    k <- k + 1L
  }
  out[seq_len(L)]
}

#' .htpinvspd
#'
#' A step of the htpfn implementation. Called by \code{Htpfn}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param A A matrix; passed to \code{nrow}.
#' @param ridge Numeric; combined arithmetically in the body.
#' @return The value of \code{out}, as built in the body.
#' @export
.htpinvspd <- function(A, ridge) {
  n <- nrow(A)
  B <- A
  diag(B) <- diag(B) + ridge
  out <- matrix(0, n, n)
  for (j in seq_len(n)) {
    e <- numeric(n)
    e[j] <- 1
    out[, j] <- .s03cholsolve(B, e)
  }
  out
}
