# SPDX-License-Identifier: AGPL-3.0-or-later
#' Ridge regression in the MASS::lm.ridge parameterisation
#'
#' beta(lambda) = (X'X + lambda I)^-1 X' y with y centred and each predictor
#' centred and divided by its root-mean-square deviation (divisor n), the
#' coefficients divided back by those scales and the intercept
#' mean(y) - sum(beta mean(X)).  Source consulted: Hoerl and Kennard (1970),
#' Technometrics 12(1), 55-67, equation (3.4).  Verified against MASS::lm.ridge.
#'
#' @param X predictor matrix without an intercept column.
#' @param y response.
#' @param lam ridge penalty.
#' @return list: estimate, coefficients, intercept, scales, gcv, df, lam,
#'   n, method.
#' @keywords internal
#' @examples
#' ridgrg(cbind(1:10, c(2, 1, 4, 3, 6, 5, 8, 7, 10, 9)),
#'        c(1.2, 2.3, 2.9, 4.1, 5.2, 5.8, 7.3, 8.1, 8.9, 10.2), 0.5)$intercept
#' @export
ridgrg <- function(X, y, lam = 0) {
  m <- as.matrix(X)
  yv <- as.numeric(y)
  n <- length(yv)
  if (nrow(m) != n) m <- t(m)
  dimnames(m) <- NULL
  q <- ncol(m)
  xm <- colMeans(m)
  xc <- sweep(m, 2, xm, "-")
  xs <- sqrt(colSums(xc^2) / n)
  xsc <- sweep(xc, 2, xs, "/")
  ym <- mean(yv)
  yc <- yv - ym
  a <- crossprod(xsc) + lam * diag(q)
  bs <- as.numeric(solve(a, crossprod(xsc, yc)))
  beta <- bs / xs
  b0 <- ym - sum(beta * xm)
  hatd <- xsc %*% solve(a, t(xsc))
  df <- sum(diag(hatd))
  resid <- as.numeric(yc - xsc %*% bs)
  gcv <- sum(resid * resid) / (n * (1 - df / n)^2)
  list(estimate = c(b0, beta), coefficients = beta, intercept = b0,
       scales = xs, gcv = gcv, df = df, lam = as.numeric(lam), n = n,
       method = "Ridge regression, lm.ridge scaling (Hoerl & Kennard 1970, eq. 3.4)")
}

# CANONICAL TEST
# r <- ridgrg(cbind(1:10, c(2,1,4,3,6,5,8,7,10,9)),
#             c(1.2,2.3,2.9,4.1,5.2,5.8,7.3,8.1,8.9,10.2), 0.5)
# stopifnot(abs(r$intercept - 0.291850723533891) < 1e-10)

#' @rdname ridgrg
#' @keywords internal
#' @export
morie_ridgrg <- ridgrg
