# SPDX-License-Identifier: AGPL-3.0-or-later

# Internal: deterministic nested-grid minimiser on a rectangle. Both
# language arms run the identical arithmetic -- a fixed 21x21 sweep,
# then 7 zooms each shrinking the box by ten -- so they land on the same
# numbers rather than on two different optimiser trajectories.
.sarmix_refine <- function(negll, lo1, hi1, lo2, hi2) {
  levels <- 7L
  ngrid <- 21L
  a1 <- lo1; b1 <- hi1; a2 <- lo2; b2 <- hi2
  best <- c(Inf, a1, a2)
  for (lev in seq_len(levels)) {
    s1 <- (b1 - a1) / (ngrid - 1L)
    s2 <- (b2 - a2) / (ngrid - 1L)
    best <- c(Inf, a1, a2)
    for (i in seq_len(ngrid)) {
      u <- a1 + (i - 1L) * s1
      for (j in seq_len(ngrid)) {
        v <- a2 + (j - 1L) * s2
        f <- negll(u, v)
        # Strict-improvement threshold, not `<`. The two language arms
        # evaluate this likelihood to ~1e-15 of each other; on a flat
        # ridge a bare `<` lets that last bit decide the argmin and the
        # arms walk to adjacent lattice points. A 1e-12 margin means only
        # a real improvement moves the incumbent, and both arms scan in
        # the same order, so both keep the same point.
        if (f < best[1] - 1e-12) best <- c(f, u, v)
      }
    }
    a1n <- best[2] - s1; b1n <- best[2] + s1
    a2n <- best[3] - s2; b2n <- best[3] + s2
    a1 <- max(a1n, lo1); b1 <- min(b1n, hi1)
    a2 <- max(a2n, lo2); b2 <- min(b2n, hi2)
  }
  best
}

#' Combined spatial autoregressive lag + autoregressive error (SARAR).
#'
#' Model (Kelejian & Prucha 1998, eqs. (1)-(2), p. 101):
#' \code{y = rho W1 y + X beta + u}, \code{u = lam W2 u + eps},
#' \code{eps ~ N(0, sigma2 I)}.
#'
#' Writing \code{A = I - rho W1} and \code{B = I - lam W2}, the
#' transformed system \code{B A y = B X beta + eps} is spherical, so for
#' fixed \code{(rho, lam)} the remaining parameters are ordinary least
#' squares on \code{ystar = B A y} against \code{Xstar = B X}, and the
#' profile log-likelihood is
#' \code{-n/2 log(2 pi sigma2) + log|A| + log|B| - n/2}, maximised over
#' the admissible rectangle. Each parameter's admissible range is the
#' eigenvalue interval on which \code{|I - t W|} stays positive
#' (Schabenberger & Gotway 2005, eq. 6.48, p. 340) -- not a hardcoded
#' (-0.99, 0.99), which is wrong for a raw adjacency.
#'
#' \code{lam = 0} recovers the SAR lag model (\code{\link{sarla}}) and
#' \code{rho = 0} the SAR error model (\code{\link{sarre}}). Passing the
#' same matrix for W1 and W2 gives the SAC model (\code{\link{Sacmod}}).
#'
#' @param y Response, length n.
#' @param X Design matrix (n by p); the intercept must be explicit.
#' @param W1 Weights for the autoregressive lag in the response.
#' @param W2 Weights for the autoregressive disturbance.
#' @return Named list: estimate, se, rho, lambda, sigma2, loglik, n, method.
#' @references Kelejian, H. H. and Prucha, I. R. (1998). A generalized
#'   spatial two-stage least squares procedure for estimating a spatial
#'   autoregressive model with autoregressive disturbances. The Journal
#'   of Real Estate Finance and Economics 17(1), 99-121.
#'   \doi{10.1023/A:1007707430416}.
#'   Anselin, L. (1988). Spatial Econometrics: Methods and Models.
#'   Schabenberger, O. and Gotway, C. A. (2005), Sec. 6.2.2, pp. 335-341.
#' @examples
#' # See the package vignettes for usage examples:
#' #   vignette(package = "morie")
#' @export
Sarmix <- function(y, X, W1, W2) {
  Xm <- as.matrix(X)
  yv <- as.numeric(y)
  A1 <- as.matrix(W1)
  A2 <- as.matrix(W2)
  n <- nrow(Xm); p <- ncol(Xm)
  if (length(yv) != n || any(dim(A1) != c(n, n)) || any(dim(A2) != c(n, n)))
    stop("shape mismatch among y, X, W1, W2")
  if (n <= p) stop("need more observations than columns of X")
  I <- diag(n)

  parts <- function(rho, lam) {
    A <- I - rho * A1
    B <- I - lam * A2
    ystar <- as.numeric(B %*% (A %*% yv))
    Xstar <- B %*% Xm
    G <- crossprod(Xstar)
    beta <- as.numeric(solve(G, crossprod(Xstar, ystar)))
    e <- ystar - as.numeric(Xstar %*% beta)
    list(A = A, B = B, G = G, beta = beta, e = e)
  }

  negll <- function(rho, lam) {
    pt <- try(parts(rho, lam), silent = TRUE)
    if (inherits(pt, "try-error")) return(1e12)
    s2 <- sum(pt$e^2) / n
    if (!(s2 > 0)) return(1e12)
    da <- determinant(pt$A, logarithm = TRUE)
    db <- determinant(pt$B, logarithm = TRUE)
    if (da$sign <= 0 || db$sign <= 0 ||
        !is.finite(da$modulus) || !is.finite(db$modulus)) return(1e12)
    0.5 * n * log(2 * pi * s2) - as.numeric(da$modulus) -
      as.numeric(db$modulus) + 0.5 * n
  }

  iv1 <- .sp_rho_interval(A1, "identity")
  iv2 <- .sp_rho_interval(A2, "identity")
  best <- .sarmix_refine(negll, iv1[1], iv1[2], iv2[1], iv2[2])
  rho <- best[2]; lam <- best[3]

  pt <- parts(rho, lam)
  sigma2 <- sum(pt$e^2) / max(n - p, 1)
  cov_b <- sigma2 * solve(pt$G)
  se <- sqrt(pmax(diag(cov_b), 0))

  list(
    estimate = pt$beta, se = se, rho = rho, lambda = lam,
    sigma2 = sigma2, loglik = -best[1], n = n,
    method = "SARAR (SAR lag + SAR error) by concentrated ML"
  )
}

#' @rdname Sarmix
#' @keywords internal
#' @export
morie_spatial_ar_combined <- Sarmix
