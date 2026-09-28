.gwx_w <- function(P, q, bw, kernel, adaptive) {
  GWRKernelWeights(sqrt((P[, 1] - q[1])^2 + (P[, 2] - q[2])^2), bw, kernel, adaptive)
}

.gwx_wcor <- function(x, y, w) {
  if (all(x == x[1]) || all(y == y[1])) return(NaN)
  mx <- sum(w * x)
  my <- sum(w * y)
  sxx <- sum(w * (x - mx)^2)
  syy <- sum(w * (y - my)^2)
  if (sxx > 0 && syy > 0) sum(w * (x - mx) * (y - my)) / sqrt(sxx * syy) else NaN
}

.gwx_pf <- function(q, d1, d2, lower) {
  p <- if (is.infinite(d1) && is.infinite(d2)) as.numeric(q >= 1) else if (is.infinite(d1)) {
    if (q > 0) stats::pchisq(d2 / q, d2, lower.tail = FALSE) else 0
  } else if (is.infinite(d2)) stats::pchisq(d1 * q, d1) else stats::pf(q, d1, d2)
  if (lower) p else 1 - p
}

#' GWR collinearity diagnostics, prediction, F tests and GW summary statistics
#'
#' \code{GwrCollinearity}: local weighted correlations, VIFs, condition
#' numbers and variance-decomposition proportions, as
#' \code{GWmodel::gwr.collin.diagno}. \code{GwrPredict}: GWR prediction with
#' prediction variance, as \code{GWmodel::gwr.predict}. \code{GwrFTests}:
#' Leung, Mei and Zhang's F1 to F4 tests (\code{method = "gwmodel"}
#' reproduces GWmodel 2.4's simplifications). \code{GwSummary}:
#' geographically weighted means, variances, skewness, CVs, covariances and
#' Pearson and Spearman correlations, as \code{GWmodel::gwss}. Identical to
#' the Python arm \code{morie.fn.gwrext}.
#'
#' @param y Response.
#' @param X Design (intercept first) or data columns (\code{GwSummary}).
#' @param coords Two-column coordinates.
#' @param bw Bandwidth.
#' @param kernel Kernel (as \code{GWRKernelWeights}).
#' @param adaptive Adaptive bandwidth.
#' @param X0,coords0 Prediction design and coordinates.
#' @param method \code{"leung"} or \code{"gwmodel"}.
#' @return List.
#' @references Wheeler, D. C. (2007). Diagnostic tools and a remedial method
#'   for collinearity in geographically weighted regression. Environment and
#'   Planning A 39, 2464-2481.
#'
#'   Leung, Y., Mei, C.-L. and Zhang, W.-X. (2000). Statistical tests for
#'   spatial nonstationarity based on the geographically weighted regression
#'   model. Environment and Planning A 32, 9-32.
#'
#'   Brunsdon, C., Fotheringham, A. S. and Charlton, M. (2002).
#'   Geographically weighted summary statistics. Computers, Environment and
#'   Urban Systems 26, 501-524.
#' @examples
#' X <- cbind(1, 0:4)
#' GwrPredict(c(1, 3, 5, 7, 9), X, cbind(0:4, 0), rbind(c(1, 2.5)), rbind(c(2.5, 0)), 100)$prediction
#' GwSummary(cbind(1:3, c(2, 4, 6)), cbind(0:2, 0), 100)$corr
#' @export
GwrCollinearity <- function(X, coords, bw, kernel = "bisquare", adaptive = FALSE) {
  X <- unname(as.matrix(X) * 1)
  P <- as.matrix(coords)
  n <- nrow(X)
  k <- ncol(X)
  res <- lapply(seq_len(n), function(i) {
    w <- .gwx_w(P, P[i, ], bw, kernel, adaptive)
    w <- w / sum(w)
    pr <- which(upper.tri(diag(k)), arr.ind = TRUE)
    pr <- pr[order(pr[, 1], pr[, 2]), , drop = FALSE]
    cr <- apply(pr, 1, function(ab) .gwx_wcor(X[, ab[1]], X[, ab[2]], w))
    R <- outer(2:k, 2:k, Vectorize(function(a, b) .gwx_wcor(X[, a], X[, b], w)))
    xw <- X * w
    s <- svd(sweep(xw, 2, sqrt(colSums(xw^2)), "/"))
    phi <- t((s$v %*% diag(1 / s$d, k))^2)
    list(cr = cr, vif = diag(solve(R)), cn = s$d[1] / s$d[k], vdp = (phi / rep(colSums(phi), each = k))[k, ])
  })
  list(corr = lapply(res, `[[`, "cr"), vif = lapply(res, `[[`, "vif"), local_cn = vapply(res, `[[`, 0, "cn"),
       vdp = lapply(res, `[[`, "vdp"))
}

#' @rdname GwrCollinearity
#' @export
GwrPredict <- function(y, X, coords, X0, coords0, bw, kernel = "bisquare", adaptive = FALSE) {
  X <- as.matrix(X) * 1
  P <- as.matrix(coords)
  n <- nrow(X)
  S <- t(vapply(seq_len(n), function(i) {
    w <- .gwx_w(P, P[i, ], bw, kernel, adaptive)
    as.vector(X[i, ] %*% solve(crossprod(X, w * X), t(X * w)))
  }, numeric(n)))
  s2 <- sum((y - S %*% y)^2) / (n - 2 * sum(diag(S)) + sum(S^2))
  X0 <- as.matrix(X0)
  Q <- as.matrix(coords0)
  out <- lapply(seq_len(nrow(Q)), function(k) {
    w <- .gwx_w(P, Q[k, ], bw, kernel, adaptive)
    A <- solve(crossprod(X, w * X))
    b <- as.vector(A %*% crossprod(X, w * y))
    s0 <- A %*% crossprod(X, w^2 * X) %*% A
    list(b = b, p = sum(X0[k, ] * b), v = s2 * (1 + as.numeric(t(X0[k, ]) %*% s0 %*% X0[k, ])))
  })
  list(betas = lapply(out, `[[`, "b"), prediction = vapply(out, `[[`, 0, "p"), variance = vapply(out, `[[`, 0, "v"),
       sigma2 = s2)
}

#' @rdname GwrCollinearity
#' @export
GwrFTests <- function(y, X, coords, bw, kernel = "bisquare", adaptive = FALSE, method = "leung") {
  if (!method %in% c("leung", "gwmodel")) stop("method must be leung or gwmodel", call. = FALSE)
  X <- as.matrix(X) * 1
  P <- as.matrix(coords)
  n <- nrow(X)
  k <- ncol(X)
  Cs <- lapply(seq_len(n), function(i) {
    w <- .gwx_w(P, P[i, ], bw, kernel, adaptive)
    solve(crossprod(X, w * X), t(X * w))
  })
  betas <- t(vapply(Cs, function(C) as.vector(C %*% y), numeric(k)))
  S <- t(vapply(seq_len(n), function(i) as.vector(X[i, ] %*% Cs[[i]]), numeric(n)))
  IS <- diag(n) - S
  R <- crossprod(IS)
  d1 <- sum(diag(R))
  d2 <- if (method == "gwmodel") 0 else sum(R * t(R))
  rss_g <- sum((y - S %*% y)^2)
  rss_o <- sum(stats::lm.fit(X, y)$residuals^2)
  dfo <- n - k
  f1 <- (rss_g / d1) / (rss_o / dfo)
  f1df <- c(if (d2 > 0) d1^2 / d2 else Inf, dfo)
  f2 <- ((rss_o - rss_g) / (dfo - d1)) / (rss_o / dfo)
  f2df <- c((dfo - d1)^2 / (dfo - 2 * d1 + d2), dfo)
  s2d <- rss_g / d1
  f3 <- lapply(seq_len(k), function(a) {
    B <- t(vapply(Cs, function(C) C[a, ], numeric(n)))
    Bc <- sweep(B, 2, colMeans(B))
    BJ <- crossprod(Bc) / n
    g1 <- sum(diag(BJ))
    g2 <- if (method == "gwmodel") sum(diag(BJ)^2) else sum(BJ * t(BJ))
    vk2 <- sum((betas[, a] - mean(betas[, a]))^2) / n
    st <- (vk2 / g1) / s2d
    df <- c(g1^2 / g2, f1df[1])
    list(st = st, df = df, p = .gwx_pf(st, df[1], df[2], FALSE))
  })
  f4 <- rss_g / rss_o
  list(F1 = f1, F1_df = f1df, F1_p = .gwx_pf(f1, f1df[1], f1df[2], TRUE), F2 = f2, F2_df = f2df,
       F2_p = .gwx_pf(f2, f2df[1], f2df[2], FALSE), F3 = vapply(f3, `[[`, 0, "st"), F3_df = lapply(f3, `[[`, "df"),
       F3_p = vapply(f3, `[[`, 0, "p"), F4 = f4, F4_df = c(d1, dfo), F4_p = .gwx_pf(f4, d1, dfo, TRUE), delta1 = d1,
       delta2 = d2, rss_gwr = rss_g, rss_ols = rss_o, betas = betas)
}

#' @rdname GwrCollinearity
#' @export
GwSummary <- function(X, coords, bw, kernel = "bisquare", adaptive = FALSE) {
  X <- unname(as.matrix(X) * 1)
  P <- as.matrix(coords)
  n <- nrow(X)
  k <- ncol(X)
  rk <- apply(X, 2, rank)
  pr <- which(upper.tri(diag(k)), arr.ind = TRUE)
  pr <- pr[order(pr[, 1], pr[, 2]), , drop = FALSE]
  res <- lapply(seq_len(n), function(i) {
    w <- .gwx_w(P, P[i, ], bw, kernel, adaptive)
    w <- w / sum(w)
    m <- colSums(w * X)
    v <- colSums(w * sweep(X, 2, m)^2)
    sd <- sqrt(v)
    cw <- 1 - sum(w^2)
    list(m = m, v = v, sd = sd, sk = colSums(w * sweep(X, 2, m)^3) / sd^3, cv = sd / m,
         cov = apply(pr, 1, function(ab) sum(w * (X[, ab[1]] - m[ab[1]]) * (X[, ab[2]] - m[ab[2]])) / cw),
         cor = apply(pr, 1, function(ab) .gwx_wcor(X[, ab[1]], X[, ab[2]], w)),
         sp = apply(pr, 1, function(ab) .gwx_wcor(rk[, ab[1]], rk[, ab[2]], w)))
  })
  g <- function(key) lapply(res, `[[`, key)
  list(mean = g("m"), sd = g("sd"), var = g("v"), skewness = g("sk"), cv = g("cv"), cov = g("cov"), corr = g("cor"),
       spearman = g("sp"))
}
