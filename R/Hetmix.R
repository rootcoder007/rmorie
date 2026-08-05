# SPDX-License-Identifier: AGPL-3.0-or-later
#' Basic reproduction number under heterogeneous mixing
#'
#' R0 is the spectral radius of the next generation matrix.  The
#' dominant eigenvalue is found by power iteration, which converges for
#' a non-negative primitive matrix by Perron-Frobenius; the same theorem
#' is why the dominant eigenvector -- the stable distribution of
#' infections across groups -- can be reported with a positive sign.
#' Homogeneous mixing collapses to R0 = c n / gamma, the closed form the
#' tests check.
#'
#' Formula: R0 = rho(C diag(1/gamma)).
#'
#' @param contact_matrix Square non-negative transmission matrix.
#' @param gamma Per-group removal rates, or one shared rate.
#' @param iters Power iteration cap.
#' @param tol Convergence tolerance on the eigenvalue.
#' @return List with \code{estimate}, \code{R0},
#'   \code{stable_distribution}, \code{iterations}, \code{epidemic},
#'   \code{n}, \code{method}.
#' @references Diekmann and Heesterbeek (2000), Mathematical
#'   Epidemiology of Infectious Diseases, Wiley, ch. 5; Diekmann,
#'   Heesterbeek and Metz (1990), Journal of Mathematical Biology
#'   28(4):365-382. \doi{10.1007/BF00178324}
#' @export
Hetmix <- function(contact_matrix, gamma, iters = 2000, tol = 1e-14) {
  C <- .s03mat(contact_matrix)
  n <- nrow(C)
  if (n == 0L) stop("heterogeneous_mixing: contact matrix is empty")
  if (ncol(C) != n) stop("heterogeneous_mixing: contact matrix must be square")
  if (any(C < 0)) stop("heterogeneous_mixing: contact rates must be non-negative")
  g <- .s03vec(gamma)
  if (length(g) == 1L && n > 1L) g <- rep(g, n)
  if (length(g) != n) stop("heterogeneous_mixing: gamma must have one rate per group")
  if (any(g <= 0)) stop("heterogeneous_mixing: removal rates must be positive")
  K <- C %*% diag(1 / g, n)
  x <- rep(1 / n, n)
  lam <- 0; it <- 0L
  for (k in seq_len(as.integer(iters))) {
    y <- as.numeric(K %*% x)
    nrm <- sqrt(sum(y * y))
    if (nrm == 0) { lam <- 0; break }
    y <- y / nrm
    new <- sum(y * as.numeric(K %*% y))
    it <- it + 1L
    if (abs(new - lam) <= tol) { lam <- new; x <- y; break }
    lam <- new; x <- y
  }
  if (sum(x) != 0) x <- x / sum(x)
  .t1_result(estimate = lam, R0 = lam, stable_distribution = x, iterations = it,
             epidemic = as.integer(lam > 1), n = n,
             method = "spectral radius of K = C diag(1/gamma), Diekmann & Heesterbeek (2000) ch. 5")
}
