.spdiag_data <- function() {
  n <- 8
  ii <- 0:(n - 1)
  B <- outer(ii, ii, function(i, j) as.numeric(abs(i - j) == 1))
  B[1, 8] <- B[8, 1] <- B[3, 6] <- B[6, 3] <- 1
  W <- B / rowSums(B)
  X <- cbind(1, ((ii * 7) %% 11) / 5)
  y <- as.vector(1 + 2 * X[, 2] + ((ii * 3) %% 5 - 2) / 4 + 0.3 * W %*% X[, 2])
  e <- ((ii * 4) %% 7 - 3) / 5
  list(n = n, W = W, X = X, y = y, e = e, W3 = matrix(c(0, .5, 0, 1, 0, 1, 0, .5, 0), 3))
}

.spdiag_fisher <- function(X, W, beta, rho, lam, s2, lag, err) {
  # I_ij = dmu_i' S^-1 dmu_j + tr(S^-1 dS_i S^-1 dS_j) / 2 for the Gaussian SAC likelihood
  n <- nrow(X)
  Ai <- solve(diag(n) - rho * W)
  Bi <- solve(diag(n) - lam * W)
  S <- Ai %*% Bi
  Sig <- s2 * S %*% t(S)
  Si <- solve(Sig)
  dmu <- lapply(seq_len(ncol(X)), function(k) Ai %*% X[, k])
  dsg <- lapply(seq_len(ncol(X)), function(k) matrix(0, n, n))
  ds <- function(dS) s2 * (dS %*% t(S) + S %*% t(dS))
  if (lag) {
    AWA <- Ai %*% W %*% Ai
    dmu <- c(dmu, list(AWA %*% X %*% beta))
    dsg <- c(dsg, list(ds(AWA %*% Bi)))
  }
  if (err) {
    dmu <- c(dmu, list(matrix(0, n, 1)))
    dsg <- c(dsg, list(ds(S %*% W %*% Bi)))
  }
  dmu <- c(dmu, list(matrix(0, n, 1)))
  dsg <- c(dsg, list(S %*% t(S)))
  k <- length(dmu)
  outer(seq_len(k), seq_len(k), Vectorize(function(a, b) {
    as.numeric(t(dmu[[a]]) %*% Si %*% dmu[[b]]) + 0.5 * sum(diag(Si %*% dsg[[a]] %*% Si %*% dsg[[b]]))
  }))
}

.spdiag_moran <- function(e, W) length(e) / sum(W) * sum(e * (W %*% e)) / sum(e^2)
