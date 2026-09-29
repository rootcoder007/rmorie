.lmt_design <- function(X, n, intercept = TRUE) {
  X <- unname(as.matrix(X)) * 1
  if (nrow(X) != n) X <- matrix(X, n)
  const <- which(apply(X, 2, function(v) length(unique(v)) == 1 && v[1] != 0))
  if (length(const) == 0 && intercept) {
    X <- cbind(1, X)
    const <- 1L
  }
  list(X = X, const = const)
}

.lmt_core <- function(y, X, W, intercept = TRUE) {
  y <- as.numeric(y)
  n <- length(y)
  d <- .lmt_design(X, n, intercept)
  X <- d$X
  W <- unname(as.matrix(W)) * 1
  k <- ncol(X)
  xtxi <- solve(crossprod(X))
  beta <- as.vector(xtxi %*% crossprod(X, y))
  predy <- as.vector(X %*% beta)
  u <- y - predy
  s2 <- sum(u * u) / n
  wu <- as.vector(W %*% u)
  wy <- as.vector(W %*% y)
  wxb <- as.vector(W %*% predy)
  tt <- sum(W * W) + sum(W * t(W))
  xwxb <- as.vector(crossprod(X, wxb))
  j <- (sum(wxb * wxb) - sum(xwxb * (xtxi %*% xwxb)) + tt * s2) / (n * s2)
  nj <- n * j
  d_err <- sum(u * wu) / s2
  d_lag <- sum(u * wy) / s2
  out <- list(n = n, k = k, T = tt, J = j, sigma2 = s2, residuals = u, coefficients = beta)
  out$RSerr <- d_err^2 / tt
  out$RSlag <- d_lag^2 / nj
  out$adjRSerr_z <- (d_err - tt * d_lag / nj) / sqrt(tt * (1 - tt / nj))
  out$adjRSerr <- out$adjRSerr_z^2
  out$adjRSlag <- (d_lag - d_err)^2 / (nj - tt)
  out$SARMA <- out$adjRSlag + out$RSerr
  xcols <- setdiff(seq_len(k), d$const)
  kx <- length(xcols)
  if (kx > 0) {
    WX <- W %*% X[, xcols, drop = FALSE]
    xtwx <- crossprod(X, WX)
    xqx <- crossprod(WX) - t(xtwx) %*% xtxi %*% xtwx
    g <- as.vector(crossprod(WX, u))
    out$RSWX <- sum(g * solve(xqx, g)) / s2
    C <- cbind(xwxb, xtwx)
    wxbwx <- as.vector(crossprod(WX, wxb))
    J22 <- rbind(c(sum(wxb * wxb) + tt * s2, wxbwx), cbind(wxbwx, crossprod(WX)))
    dd <- c(d_lag, g / s2)
    out$RSjoint_durbin <- sum(dd * solve(J22 - t(C) %*% xtxi %*% C, dd)) * s2
    out$adjRSWX <- out$RSjoint_durbin - out$RSlag
    out$adjRSlag_durbin <- out$RSjoint_durbin - out$RSWX
    out$RSerr_WX <- out$RSerr + out$RSWX
  }
  out$kx <- kx
  out
}

.lmt_res <- function(stat, df, ...) {
  c(list(statistic = stat, p_value = stats::pchisq(stat, df, lower.tail = FALSE), df = df), list(...))
}

#' Lagrange multiplier (Rao score) tests for spatial dependence after OLS
#'
#' From the OLS fit of `y` on `X` (an intercept is prepended when `X` has no
#' constant column), with residuals `u`, `sigma2 = u'u/n`,
#' `T = tr((W' + W) W)`, `nJ = ((WXb)'M(WXb) + T sigma2) / sigma2`,
#' `d_err = u'Wu / sigma2` and `d_lag = u'Wy / sigma2`: `lmerr` is
#' `RSerr = d_err^2 / T`, `lmlag` is `RSlag = d_lag^2 / nJ`, `lmrerr` and
#' `lmrlag` the robust `adjRSerr` and `adjRSlag` of Anselin, Bera, Florax and
#' Yoon (1996), `lmsarma` and `lmjoint` the joint `SARMA = adjRSlag + RSerr`
#' (2 df), and `lmdiag` all five (as `spdep::lm.RStests`).  `lmrerr2` is the
#' one-sided standard normal form of the Bera-Yoon robust error score.
#' `lmslx` is the Koley-Bera score test for the lagged regressors `WX`
#' (`k_x` df) and `lmsdm` the joint spatial Durbin test of `rho = 0` and
#' `gamma = 0` (`1 + k_x` df), as `spdep::SD.RStests`.  `lmkp` is the
#' Kelejian-Prucha (2001) Moran-type test for spatial error in the spatial
#' lag model estimated by spatial 2SLS, in the Anselin-Kelejian form of
#' `miiv`.  `sphet` is the studentised Breusch-Pagan (Koenker) `n R^2` of
#' the squared residuals on `(1, Z)`, `Z` defaulting to `W e^2`.
#'
#' @param y Numeric response.
#' @param X Regressor matrix.
#' @param W Spatial weights matrix.
#' @param residuals Residual vector.
#' @param Z Optional matrix of variance regressors; defaults to the spatial
#'   lag of the squared residuals.
#' @return A list with `statistic`, `p_value`, `df` and test-specific parts;
#'   `lmdiag` returns all five statistics and p-values.
#' @references Anselin, L. (1988). Lagrange multiplier test diagnostics for
#'   spatial dependence and spatial heterogeneity. Geographical Analysis 20,
#'   1-17. Anselin, L., Bera, A. K., Florax, R. and Yoon, M. J. (1996).
#'   Simple diagnostic tests for spatial dependence. Regional Science and
#'   Urban Economics 26, 77-104. Bera, A. K. and Yoon, M. J. (1993).
#'   Specification testing with locally misspecified alternatives.
#'   Econometric Theory 9, 649-658. Koley, M. and Bera, A. K. (2024). To use,
#'   or not to use the spatial Durbin model? Spatial Economic Analysis 19,
#'   30-56. Kelejian, H. H. and Prucha, I. R. (2001). On the asymptotic
#'   distribution of the Moran I test statistic with applications. Journal of
#'   Econometrics 104, 219-257. Koenker, R. (1981). A note on studentizing a
#'   test for heteroscedasticity. Journal of Econometrics 17, 107-112.
#' @examples
#' W <- rbind(c(0, 1, 0, 0, 0), c(0.5, 0, 0.5, 0, 0), c(0, 0.5, 0, 0.5, 0),
#'            c(0, 0, 0.5, 0, 0.5), c(0, 0, 0, 1, 0))
#' lmdiag(c(1, 2.5, 2, 4.5, 4), matrix(0:4), W)$RSerr
#' @export
lmdiag <- function(y, X, W) {
  c0 <- .lmt_core(y, X, W)
  df <- c(RSerr = 1, RSlag = 1, adjRSerr = 1, adjRSlag = 1, SARMA = 2)
  out <- list(statistic = c0$RSerr, p_value = stats::pchisq(c0$RSerr, 1, lower.tail = FALSE))
  for (nm in names(df)) {
    out[[nm]] <- c0[[nm]]
    out[[paste0("p_", nm)]] <- stats::pchisq(c0[[nm]], df[[nm]], lower.tail = FALSE)
  }
  out
}

#' @rdname lmdiag
#' @export
lmerr <- function(y, X, W) .lmt_res(.lmt_core(y, X, W)$RSerr, 1)

#' @rdname lmdiag
#' @export
lmlag <- function(y, X, W) .lmt_res(.lmt_core(y, X, W)$RSlag, 1)

#' @rdname lmdiag
#' @export
lmrerr <- function(y, X, W) .lmt_res(.lmt_core(y, X, W)$adjRSerr, 1)

#' @rdname lmdiag
#' @export
lmrlag <- function(y, X, W) .lmt_res(.lmt_core(y, X, W)$adjRSlag, 1)

#' @rdname lmdiag
#' @export
lmsarma <- function(y, X, W) .lmt_res(.lmt_core(y, X, W)$SARMA, 2)

#' @rdname lmdiag
#' @export
lmjoint <- function(y, X, W) {
  c0 <- .lmt_core(y, X, W)
  .lmt_res(c0$SARMA, 2, RSlag_plus_adjRSerr = c0$RSlag + c0$adjRSerr)
}

#' @rdname lmdiag
#' @export
lmrerr2 <- function(y, X, W) {
  c0 <- .lmt_core(y, X, W)
  list(statistic = c0$adjRSerr_z, p_value = stats::pnorm(c0$adjRSerr_z, lower.tail = FALSE), adjRSerr = c0$adjRSerr)
}

#' @rdname lmdiag
#' @export
lmslx <- function(y, X, W) {
  c0 <- .lmt_core(y, X, W)
  if (c0$kx == 0) stop("X needs at least one non-constant column")
  .lmt_res(c0$RSWX, c0$kx, RSerr_WX = c0$RSerr_WX)
}

#' @rdname lmdiag
#' @export
lmsdm <- function(y, X, W) {
  c0 <- .lmt_core(y, X, W)
  if (c0$kx == 0) stop("X needs at least one non-constant column")
  .lmt_res(c0$RSjoint_durbin, 1 + c0$kx, adjRSWX = c0$adjRSWX, adjRSlag = c0$adjRSlag_durbin,
           RSWX = c0$RSWX, RSlag = c0$RSlag)
}

#' @rdname lmdiag
#' @export
lmkp <- function(y, X, W) {
  y <- as.numeric(y)
  n <- length(y)
  d <- .lmt_design(X, n)
  W <- unname(as.matrix(W)) * 1
  xs <- d$X[, -d$const, drop = FALSE]
  fit <- morie_spatial_2sls(y, xs, W, add_intercept = TRUE)
  H <- cbind(1, xs)
  for (j in seq_len(ncol(xs))) {
    wc <- as.vector(W %*% xs[, j])
    H <- cbind(H, wc, as.vector(W %*% wc))
  }
  Z <- cbind(as.vector(W %*% y), 1, xs)
  r <- miiv(fit$residuals, W, Z, H)
  r$rho <- fit$rho
  r
}

#' @rdname lmdiag
#' @export
sphet <- function(residuals, W, Z = NULL) {
  e <- as.numeric(residuals)
  n <- length(e)
  W <- unname(as.matrix(W)) * 1
  if (nrow(W) != n || ncol(W) != n) stop("W must be (n, n)")
  e2 <- e * e
  Z <- if (is.null(Z)) matrix(as.vector(W %*% e2)) else unname(as.matrix(Z)) * 1
  D <- cbind(1, Z)
  b <- solve(crossprod(D), crossprod(D, e2))
  fit <- as.vector(D %*% b)
  stat <- n * (1 - sum((e2 - fit)^2) / sum((e2 - mean(e2))^2))
  df <- ncol(D) - 1
  list(statistic = stat, p_value = stats::pchisq(stat, df, lower.tail = FALSE), df = df,
       method = "studentised Breusch-Pagan (Koenker)", n = n)
}
