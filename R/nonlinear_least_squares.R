# SPDX-License-Identifier: AGPL-3.0-or-later
.nlsgn_jacobian <- function(model, x, theta) {
  vapply(seq_along(theta), function(j) {
    h <- 6.055454452393343e-06 * max(abs(theta[j]), 1)
    tp <- theta
    tm <- theta
    tp[j] <- tp[j] + h
    tm[j] <- tm[j] - h
    (model(x, tp) - model(x, tm)) / (2 * h)
  }, numeric(length(x)))
}

#' Nonlinear least squares by Gauss-Newton
#'
#' Minimises sum (y - model(x, theta))^2 by Gauss-Newton steps solved by QR,
#' halving each step until the residual sum of squares falls, with the
#' relative-offset convergence criterion of nls (Bates & Watts 1988);
#' standard errors are sqrt(diag(s^2 (J'J)^-1)), s^2 = RSS / (n - p)
#' (Hedderich, Sachs & Reynarowych 2023, Sec. 3.7.12). The Jacobian is by
#' central differences.
#'
#' @param model Function (x, theta) returning the fitted values for all x.
#' @param x Covariate (any object model accepts).
#' @param y Response.
#' @param start Starting values.
#' @param tol Convergence tolerance.
#' @param max_iter Iteration limit.
#' @return Named list: coefficients, se, rss, sigma, fitted, iterations,
#'   converged.
#' @references Bates, D. M. & Watts, D. G. (1988). Nonlinear Regression
#'   Analysis and Its Applications. Wiley.
#' @examples
#' nlsgn(function(x, t) t[1] * t[2]^x, 1:5, c(3, 7, 12, 26, 51), c(1, 1))$coefficients
#' @export
nlsgn <- function(model, x, y, start, tol = 1e-8, max_iter = 200) {
  y <- as.numeric(y)
  theta <- as.numeric(start)
  n <- length(y)
  p <- length(theta)
  if (n <= p) stop("need more observations than parameters", call. = FALSE)
  r <- y - model(x, theta)
  if (length(r) != n) stop("model must return one value per observation", call. = FALSE)
  rss <- sum(r^2)
  converged <- FALSE
  it <- 0L
  for (it in seq_len(max_iter)) {
    q <- qr(.nlsgn_jacobian(model, x, theta))
    delta <- qr.coef(q, r)
    rperp <- sum(qr.resid(q, r)^2)
    if (rperp <= 0 || sqrt(max(rss - rperp, 0) / p) / sqrt(rperp / (n - p)) < tol) {
      converged <- TRUE
      break
    }
    fac <- 1
    repeat {
      cand <- theta + fac * delta
      rc <- y - model(x, cand)
      if (sum(rc^2) < rss) {
        theta <- cand
        r <- rc
        rss <- sum(rc^2)
        break
      }
      fac <- fac / 2
      if (fac < 1 / 1024) {
        # the predicted Gauss-Newton decrease is below the rounding error of
        # rss itself (arm64 FMA stops just short of the tol test), so no step
        # can lower it: theta is the minimiser
        if (rss - rperp <= 4 * n * .Machine$double.eps * rss) {
          converged <- TRUE
          break
        }
        stop("step factor reduced below 1/1024 without reducing the residual sum of squares", call. = FALSE)
      }
    }
    if (converged) break
  }
  J <- .nlsgn_jacobian(model, x, theta)
  s2 <- rss / (n - p)
  list(coefficients = theta, se = sqrt(s2 * diag(solve(crossprod(J)))), rss = rss, sigma = sqrt(s2),
       fitted = model(x, theta), iterations = it, converged = converged)
}
