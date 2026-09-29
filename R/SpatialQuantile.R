.sq_simplex <- function(cost, A, b, tol = 1e-10, max_iter = 100000) {
  m <- nrow(A)
  n <- length(cost)
  sg <- ifelse(b < 0, -1, 1)
  Tb <- cbind(A * sg, diag(m), b * sg)
  basis <- n + seq_len(m) - 1
  W <- n + m
  pivot <- function(Tb, r, q) {
    Tb[r, ] <- Tb[r, ] / Tb[r, q + 1]
    for (i in seq_len(m)) if (i != r && Tb[i, q + 1] != 0) Tb[i, ] <- Tb[i, ] - Tb[i, q + 1] * Tb[r, ]
    Tb
  }
  run <- function(Tb, basis, cvec, allowed) {
    for (it in seq_len(max_iter)) {
      cb <- cvec[basis + 1]
      red <- cvec[allowed + 1] - as.vector(crossprod(Tb[, allowed + 1, drop = FALSE], cb))
      cand <- allowed[red < -tol & !(allowed %in% basis)]
      if (!length(cand)) return(list(Tb = Tb, basis = basis, status = "optimal"))
      q <- cand[1]
      rows <- which(Tb[, q + 1] > tol)
      if (!length(rows)) return(list(Tb = Tb, basis = basis, status = "unbounded"))
      ratio <- Tb[rows, W + 1] / Tb[rows, q + 1]
      best <- min(ratio)
      ok <- rows[ratio <= best + tol]
      r <- ok[order(basis[ok], ok)[1]]
      Tb <- pivot(Tb, r, q)
      basis[r] <- q
    }
    stop("simplex did not terminate")
  }
  s1 <- run(Tb, basis, c(numeric(n), rep(1, m)), seq_len(W) - 1)
  Tb <- s1$Tb
  basis <- s1$basis
  if (sum(Tb[basis >= n, W + 1]) > 1e-8 * max(1, sum(abs(b)))) return(list(x = NULL, status = "infeasible"))
  for (i in seq_len(m)) {
    if (basis[i] >= n) {
      q <- which(abs(Tb[i, seq_len(n)]) > tol)
      if (length(q)) {
        Tb <- pivot(Tb, i, q[1] - 1)
        basis[i] <- q[1] - 1
      }
    }
  }
  s2 <- run(Tb, basis, c(cost, numeric(m)), seq_len(n) - 1)
  x <- numeric(n)
  inb <- s2$basis < n
  x[s2$basis[inb] + 1] <- s2$Tb[inb, W + 1]
  list(x = x, status = s2$status)
}

#' Quantile regression by linear programming and spatial two-stage quantile regression
#'
#' \code{QuantileRegressionLp}: Koenker-Bassett regression quantile by the
#' two-phase simplex. \code{SpatialQuantileIv}: Kim-Muller two-stage quantile
#' regression of the spatial autoregressive model. Identical to the Python arm
#' \code{morie.fn.spquant}.
#'
#' @param y Response.
#' @param X Regressors (with the intercept for \code{QuantileRegressionLp},
#'   without it for \code{SpatialQuantileIv}).
#' @param tau Quantile.
#' @param W Spatial weights.
#' @param Z Optional instruments (without intercept).
#' @return A list.
#' @references Koenker, R. and Bassett, G. (1978). Regression quantiles.
#'   Econometrica 46, 33-50.
#'
#'   Kim, T.-H. and Muller, C. (2004). Two-stage quantile regression when the
#'   first stage is based on quantile regression. Econometrics Journal 7,
#'   218-231.
#' @examples
#' QuantileRegressionLp(c(1, 3, 2, 5, 4), cbind(1, 0:4))$coefficients
#' @export
QuantileRegressionLp <- function(y, X, tau = 0.5) {
  X <- as.matrix(X)
  n <- nrow(X)
  k <- ncol(X)
  A <- cbind(X, -X, diag(n), -diag(n))
  s <- .sq_simplex(c(numeric(2 * k), rep(tau, n), rep(1 - tau, n)), A, y)
  if (s$status != "optimal") stop(paste("linear programme", s$status))
  b <- s$x[seq_len(k)] - s$x[k + seq_len(k)]
  res <- as.vector(y - X %*% b)
  list(coefficients = b, residuals = res, objective = sum(ifelse(res >= 0, tau * res, (tau - 1) * res)))
}

#' @rdname QuantileRegressionLp
#' @export
SpatialQuantileIv <- function(y, X, W, tau = 0.5, Z = NULL) {
  X <- as.matrix(X)
  k <- ncol(X)
  wy <- as.vector(W %*% y)
  Zm <- if (is.null(Z)) cbind(1, X, W %*% X) else cbind(1, as.matrix(Z))
  s1 <- QuantileRegressionLp(wy, Zm, tau)
  fitted <- wy - s1$residuals
  b <- QuantileRegressionLp(y, cbind(1, X, fitted), tau)$coefficients
  list(coefficients = b[seq_len(k + 1)], rho = b[k + 2], first_stage = s1$coefficients, wy_hat = fitted)
}
