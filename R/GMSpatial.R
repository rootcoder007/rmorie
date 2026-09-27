#' Kelejian-Prucha moment system for a spatial autoregressive error
#'
#' With residuals u, \eqn{wu = Wu} and \eqn{wwu = W Wu}, the three moment
#' conditions are \eqn{G (\lambda, \lambda^2, \sigma^2)' = g} (Kelejian and
#' Prucha 1999), as \code{spatialreg:::.kpwuwu}.
#'
#' @param u Residual vector.
#' @param W Spatial weights matrix.
#' @return List with \code{G} (3 x 3), \code{g}, \code{trwpw}
#'   (\eqn{tr(W'W)}), \code{wu}, \code{wwu}.
#' @references Kelejian, H. H. and Prucha, I. R. (1999). A generalized
#'   moments estimator for the autoregressive parameter in a spatial model.
#'   International Economic Review 40, 509-533.
#' @examples
#' W <- 1 * (abs(outer(1:6, 1:6, "-")) == 1)
#' KPMoments(c(.3, -.2, .5, -.1, .4, -.6), W / rowSums(W))$g
#' @export
KPMoments <- function(u, W) {
  u <- as.numeric(u)
  W <- as.matrix(W)
  n <- length(u)
  wu <- as.vector(W %*% u)
  wwu <- as.vector(W %*% wu)
  trwpw <- sum(W^2)
  G <- cbind(c(2 * sum(u * wu), 2 * sum(wwu * wu), sum(u * wwu) + sum(wu * wu)) / n,
             -c(sum(wu * wu), sum(wwu * wwu), sum(wwu * wu)) / n,
             c(1, trwpw / n, 0))
  g <- c(sum(u * u), sum(wu * wu), sum(u * wu)) / n
  list(G = G, g = g, trwpw = trwpw, wu = wu, wwu = wwu)
}

.gm_solve <- function(G, g, interval) {
  c3 <- G[, 3]
  prof <- function(lam) {
    r <- g - G[, 1] * lam - G[, 2] * lam^2
    sum(r^2) - sum(c3 * r)^2 / sum(c3^2)
  }
  grid <- interval[1] + diff(interval) * (0:400) / 400
  k <- which.min(vapply(grid, prof, 0))
  lam <- stats::optimize(prof, c(grid[max(k - 1, 1)], grid[min(k + 1, 401)]), tol = 1e-12)$minimum
  # the profile is a quartic in lambda: polish the root of its derivative by Newton
  for (it in 1:20) {
    r <- g - G[, 1] * lam - G[, 2] * lam^2
    d1 <- -G[, 1] - 2 * G[, 2] * lam
    d2 <- -2 * G[, 2]
    cr <- sum(c3 * r)
    cd1 <- sum(c3 * d1)
    cd2 <- sum(c3 * d2)
    f1 <- 2 * sum(r * d1) - 2 * cr * cd1 / sum(c3^2)
    f2 <- 2 * (sum(d1^2) + sum(r * d2)) - 2 * (cd1^2 + cr * cd2) / sum(c3^2)
    if (f2 <= 0) break
    step <- f1 / f2
    lam <- lam - step
    if (abs(step) <= 1e-15 * max(1, abs(lam))) break
  }
  r <- g - G[, 1] * lam - G[, 2] * lam^2
  c(lam, sum(c3 * r) / sum(c3^2))
}

.gm_tsls <- function(y, yend, X, Q, robust, sig2n_k) {
  n <- length(y)
  yendp <- as.vector(Q %*% solve(crossprod(Q), crossprod(Q, yend)))
  Zp <- cbind(yendp, X)
  Z <- cbind(yend, X)
  ZZi <- solve(crossprod(Zp))
  biv <- as.vector(ZZi %*% crossprod(Zp, y))
  dimnames(ZZi) <- NULL
  e <- y - as.vector(Z %*% biv)
  sse <- sum(e^2)
  df <- if (sig2n_k) n - ncol(Z) else n
  vb <- if (is.null(robust)) ZZi * sse / df else {
    om <- e^2 * (if (robust == "HC1") n / df else 1)
    ZZi %*% crossprod(Zp, Zp * om) %*% ZZi
  }
  list(coefficients = biv, var = vb, sse = sse, residuals = e, df = df)
}

#' Spatial error model by generalised moments
#'
#' Model \eqn{y = X\beta + u}, \eqn{u = \lambda W u + e} (Kelejian and
#' Prucha 1999). The OLS residuals give the moment system of
#' \code{KPMoments}; \eqn{(\lambda, \sigma^2)} minimise
#' \eqn{|G (\lambda, \lambda^2, \sigma^2)' - g|^2} (profiled over
#' \eqn{\sigma^2}, then Brent on \eqn{\lambda}), and \eqn{\beta} is the
#' feasible GLS fit of \eqn{y - \lambda Wy} on \eqn{X - \lambda WX}. As
#' \code{spatialreg::GMerrorsar}, the coefficient covariance is
#' \eqn{s^2 (B'B)^{-1}} with \eqn{B = X - \lambda WX} and
#' \eqn{s^2 = |e - \lambda We|^2 / n} from the OLS residuals e.
#'
#' The standard error of \eqn{\lambda} follows Kelejian and Prucha
#' (2004), with \eqn{\Phi_{rs} = s^4 tr((A_r + A_r')(A_s + A_s')) / (2n)},
#' \eqn{A_2 = W'W}, \eqn{A_1 = c (W'W - (tr(W'W)/n) I)}. spatialreg uses
#' the trace for \eqn{\Phi_{11}} but the sum of all entries of the product
#' for \eqn{\Phi_{12}} and \eqn{\Phi_{22}}. \code{lambda_se_method =
#' "trace"} (default) uses the trace throughout, as Kelejian and Prucha
#' (2004); \code{"spatialreg"} reproduces spatialreg's entry sums.
#'
#' @param y Response.
#' @param X Design matrix, including any intercept column.
#' @param W Spatial weights matrix.
#' @param se_lambda Also compute the standard error of \eqn{\lambda}.
#' @param lambda_se_method \code{"trace"} or \code{"spatialreg"}.
#' @param interval Search interval for \eqn{\lambda}.
#' @return List with \code{lambda}, \code{sigma2_gm}, \code{coefficients},
#'   \code{se}, \code{s2}, \code{lambda_se}, \code{fitted},
#'   \code{residuals}.
#' @references Kelejian, H. H. and Prucha, I. R. (1999). A generalized
#'   moments estimator for the autoregressive parameter in a spatial model.
#'   International Economic Review 40, 509-533.
#'
#'   Kelejian, H. H. and Prucha, I. R. (2004). Estimation of simultaneous
#'   systems of spatially interrelated cross sectional equations. Journal of
#'   Econometrics 118, 27-50.
#' @examples
#' W <- 1 * (abs(outer(1:8, 1:8, "-")) == 1)
#' W <- W / rowSums(W)
#' X <- cbind(1, c(.1, .6, .2, .9, .3, .5, .8, .4))
#' GMErrorSAR(c(1, 2.2, 1.4, 3.1, .9, 2, 2.6, 1.1), X, W)$lambda
#' @export
GMErrorSAR <- function(y, X, W, se_lambda = TRUE, lambda_se_method = c("trace", "spatialreg"),
                       interval = c(-0.999, 0.999)) {
  lambda_se_method <- match.arg(lambda_se_method)
  y <- as.numeric(y)
  X <- as.matrix(X)
  W <- as.matrix(W)
  n <- length(y)
  e <- as.vector(y - X %*% solve(crossprod(X), crossprod(X, y)))
  m <- KPMoments(e, W)
  s <- .gm_solve(m$G, m$g, interval)
  lam <- s[1]
  B <- X - lam * (W %*% X)
  yt <- y - lam * as.vector(W %*% y)
  beta <- as.vector(solve(crossprod(B), crossprod(B, yt)))
  fit <- as.vector(X %*% beta)
  et <- e - lam * m$wu
  s2 <- sum(et^2) / n
  cov <- solve(crossprod(B)) * s2
  lse <- NULL
  if (se_lambda) {
    a <- m$trwpw / n
    cc <- sqrt(1 / (1 + a^2))
    wu <- m$wu
    wwu <- m$wwu
    J <- matrix(c(2 * cc * (sum(wwu * wu) - a * sum(wu * e)), sum(wwu * e) + sum(wu * wu),
                  -cc * (sum(wwu * wwu) - a * sum(wu * wu)), -sum(wwu * wu)), 2, 2) / n
    J1 <- as.vector(J %*% c(1, 2 * lam))
    A2 <- crossprod(W)
    A2s <- A2 + t(A2)
    A1s <- cc * (A2s - 2 * a * diag(n))
    f12 <- if (lambda_se_method == "trace") function(P, Q) sum(P * t(Q)) else function(P, Q) sum(rowSums(P) * rowSums(Q))
    p12 <- f12(A1s, A2s)
    phi <- s2^2 / (2 * n) * matrix(c(sum(A1s * t(A1s)), p12, p12, f12(A2s, A2s)), 2, 2)
    jj <- sum(J1^2)
    lse <- sqrt(as.numeric(t(J1) %*% phi %*% J1) / jj^2 / n)
  }
  list(lambda = lam, sigma2_gm = s[2], coefficients = beta, se = sqrt(diag(cov)), s2 = s2,
       lambda_se = lse, fitted = fit, residuals = y - fit)
}

#' SAC model by generalised spatial two-stage least squares
#'
#' Model \eqn{y = \rho W y + X\beta + u}, \eqn{u = \lambda W_2 u + e}
#' (Kelejian and Prucha 1998), as \code{spatialreg::gstsls}: 2SLS of y on
#' (Wy, X) with instruments X, WX, \eqn{W^2X} (lags of the non-intercept
#' columns); \eqn{\lambda} from the generalised moments of those residuals
#' on \eqn{W_2}; then 2SLS of \eqn{y - \lambda W_2 y} on
#' \eqn{(Wy - \lambda W_2 Wy, X - \lambda W_2 X)} with the same
#' instruments. Covariance \eqn{s^2 (Z_p'Z_p)^{-1}}, \eqn{s^2 = e'e/df}
#' (df = n unless \code{sig2n_k}), or the HC0 / HC1 sandwich.
#'
#' @param y Response.
#' @param X Design matrix; a leading column of ones is the intercept.
#' @param W Weights of the spatial lag.
#' @param W2 Weights of the error process (default W).
#' @param robust \code{NULL}, \code{"HC0"} or \code{"HC1"}.
#' @param sig2n_k Divide by \eqn{n - k}.
#' @param interval Search interval for \eqn{\lambda}.
#' @return List with \code{rho}, \code{lambda}, \code{sigma2_gm},
#'   \code{coefficients} (rho first), \code{se}, \code{cov}, \code{s2},
#'   \code{residuals}.
#' @references Kelejian, H. H. and Prucha, I. R. (1998). A generalized
#'   spatial two-stage least squares procedure for estimating a spatial
#'   autoregressive model with autoregressive disturbances. Journal of Real
#'   Estate Finance and Economics 17, 99-121.
#' @examples
#' W <- 1 * (abs(outer(1:8, 1:8, "-")) == 1)
#' W <- W / rowSums(W)
#' X <- cbind(1, c(.1, .6, .2, .9, .3, .5, .8, .4))
#' GS2SLSSAC(c(1, 2.2, 1.4, 3.1, .9, 2, 2.6, 1.1), X, W)$rho
#' @export
GS2SLSSAC <- function(y, X, W, W2 = NULL, robust = NULL, sig2n_k = FALSE, interval = c(-0.999, 0.999)) {
  y <- as.numeric(y)
  X <- as.matrix(X)
  W <- as.matrix(W)
  W2 <- if (is.null(W2)) W else as.matrix(W2)
  n <- length(y)
  p <- ncol(X)
  start <- if (all(X[, 1] == 1)) 2L else 1L
  if (start > p) stop("X needs a non-intercept column to build instruments")
  WX <- W %*% X[, start:p, drop = FALSE]
  inst <- cbind(WX, W %*% WX)
  Wy <- as.vector(W %*% y)
  u <- .gm_tsls(y, Wy, X, cbind(X, inst), robust, sig2n_k)$residuals
  m <- KPMoments(u, W2)
  s <- .gm_solve(m$G, m$g, interval)
  lam <- s[1]
  yt <- y - lam * as.vector(W2 %*% y)
  xt <- X - lam * (W2 %*% X)
  wyt <- Wy - lam * as.vector(W2 %*% Wy)
  r <- .gm_tsls(yt, wyt, xt, cbind(xt, inst), robust, sig2n_k)
  list(rho = r$coefficients[1], lambda = lam, sigma2_gm = s[2], coefficients = r$coefficients,
       se = sqrt(diag(r$var)), cov = r$var, s2 = r$sse / r$df, residuals = r$residuals)
}
