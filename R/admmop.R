# SPDX-License-Identifier: AGPL-3.0-or-later
#' ADMM in scaled form for the LASSO.
#'
#' x <- (X'X + rho I)^{-1}(X'y + rho(z - u));
#' z <- S_{lam/rho}(x + u); u <- u + x - z.
#'
#' @param X Design matrix.
#' @param y Response of length n.
#' @param lam L1 penalty, non-negative.
#' @param rho Augmented-Lagrangian parameter, strictly positive.
#' @param steps Fixed iteration count.
#'
#' @return List with x, z, u, objective, primalres, dualres, rho, steps,
#'   n, p.
#' @references Boyd, Parikh, Chu, Peleato and Eckstein (2011),
#'   Foundations and Trends in Machine Learning 3(1), Sect. 6.4 and
#'   Sect. 3.1.1.  Standard published form; the monograph is not in the
#'   local corpus and was not read.
#' @export
#' @examples
#' Admmlasso(X = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), lam = 5L)
Admmlasso <- function(X, y, lam, rho = 1, steps = 100) {
  Xm <- .t1_mat(X); y <- .t1_vec(y)
  lam <- as.numeric(lam); rho <- as.numeric(rho); steps <- as.integer(steps)
  n <- nrow(Xm); p <- ncol(Xm)
  if (n != length(y)) stop("X must have one row per entry of y")
  if (lam < 0) stop("lam must be non-negative")
  if (rho <= 0) stop("rho must be strictly positive")
  A <- crossprod(Xm) + rho * diag(p)
  dimnames(A) <- NULL
  Xty <- as.numeric(crossprod(Xm, y))
  x <- rep(0, p); z <- rep(0, p); u <- rep(0, p); dual <- 0
  for (k in seq_len(steps)) {
    x <- as.numeric(solve(A, Xty + rho * (z - u)))
    zold <- z
    z <- .k01_soft(x + u, lam / rho)
    u <- u + x - z
    dual <- rho * sqrt(sum((z - zold)^2))
  }
  res <- as.numeric(Xm %*% z) - y
  obj <- 0.5 * sum(res^2) + lam * sum(abs(z))
  .t1_result(x = x, z = z, u = u, objective = obj,
             primalres = sqrt(sum((x - z)^2)), dualres = dual,
             rho = rho, steps = steps, n = n, p = p,
             method = "ADMM for the LASSO, scaled form (Boyd et al. 2011 Sect. 6.4)")
}
