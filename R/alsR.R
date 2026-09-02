# SPDX-License-Identifier: AGPL-3.0-or-later
#' Alternating least squares for implicit-feedback matrix factorisation.
#'
#' p_ui = 1{r_ui > 0}, c_ui = 1 + alpha r_ui, and the exact alternating
#' solutions x_u = (Y'C^u Y + lam I)^{-1} Y'C^u p(u),
#' y_i = (X'C^i X + lam I)^{-1} X'C^i p(i).
#'
#' @param R Non-negative observation counts, m x n.
#' @param f Number of latent factors.
#' @param lam Ridge penalty.
#' @param alpha Confidence scaling.
#' @param steps Fixed number of alternating sweeps.
#' @param X0,Y0 Starting factors; NULL uses the deterministic defaults
#'   x_uk = ((u+k) mod 5 + 1)/10 and y_ik = ((i+2k) mod 7 + 1)/10 with
#'   zero-based u, i, k.
#'
#' @return List with X, Y, loss, fitted, m, n, f, steps.
#' @references Hu, Koren and Volinsky (2008), IEEE ICDM, 263-272,
#'   Equations (3), (4) and (5).  Read from the authors' own PDF at
#'   yifanhu.net/PUB/cf.pdf.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Alsmf(V)
Alsmf <- function(R, f = 2, lam = 0.1, alpha = 40, steps = 10,
                  X0 = NULL, Y0 = NULL) {
  Rm <- .t1_mat(R); m <- nrow(Rm); n <- ncol(Rm); f <- as.integer(f)
  if (f < 1L) stop("f must be at least 1")
  if (any(Rm < 0)) stop("counts must be non-negative")
  lam <- as.numeric(lam); al <- as.numeric(alpha)
  Cf <- 1 + al * Rm
  P <- (Rm > 0) * 1
  X <- if (is.null(X0))
    outer(seq_len(m) - 1L, seq_len(f) - 1L,
          function(u, k) ((u + k) %% 5 + 1) / 10) else .t1_mat(X0)
  Y <- if (is.null(Y0))
    outer(seq_len(n) - 1L, seq_len(f) - 1L,
          function(i, k) ((i + 2 * k) %% 7 + 1) / 10) else .t1_mat(Y0)
  dim(X) <- c(m, f); dim(Y) <- c(n, f)
  for (s in seq_len(as.integer(steps))) {
    for (u in seq_len(m)) {
      cw <- Cf[u, ]
      A <- crossprod(Y * cw, Y) + lam * diag(f)
      dimnames(A) <- NULL
      X[u, ] <- as.numeric(solve(A, as.numeric(crossprod(Y, cw * P[u, ]))))
    }
    for (i in seq_len(n)) {
      cw <- Cf[, i]
      A <- crossprod(X * cw, X) + lam * diag(f)
      dimnames(A) <- NULL
      Y[i, ] <- as.numeric(solve(A, as.numeric(crossprod(X, cw * P[, i]))))
    }
  }
  fit <- X %*% t(Y)
  dim(fit) <- c(m, n)
  loss <- sum(Cf * (P - fit)^2) + lam * (sum(X^2) + sum(Y^2))
  .t1_result(X = X, Y = Y, loss = loss, fitted = fit, m = m, n = n,
             f = f, steps = as.integer(steps),
             method = "Implicit-feedback ALS (Hu-Koren-Volinsky 2008 eqs. 3-5)")
}
