# SPDX-License-Identifier: AGPL-3.0-or-later
#' Quantize a value cache with the inner-product variant of TurboQuant.
#'
#' One bit per coordinate is spent on the sign sketch of the RESIDUAL, so
#' the scalar stage gets only b - 1 and b must be at least 2.
#'
#' Formula: idx <- Quant_mse(x) at b - 1 bits; r <- x - DeQuant_mse(idx);
#'   qjl <- sign(S . r);
#'   xtilde <- DeQuant_mse(idx) + (sqrt(pi/2)/d) ||r|| S' qjl
#'
#' @param V Value cache, one value vector per row.
#' @param b Total bits per coordinate, b >= 2.
#' @param seed Seed for the pinned rotation and projection.
#' @return List with \code{reconstruction}, \code{mse},
#'   \code{residual_norm}, \code{mean_mse}, \code{n}, \code{d}, \code{b}.
#' @references Zandieh et al., arXiv:2504.19874, Algorithm 2 lines 2-12.
#'   Fetched from arXiv.
#' @export
Vcquant <- function(V, b = 3, seed = 1) {
  V <- as.matrix(V); n <- nrow(V); d <- ncol(V); b <- as.integer(b)
  if (n < 1L) stop("the cache must hold at least one value vector")
  if (b < 2L)
    stop("b must be at least 2: one bit per coordinate goes to the sign sketch of the residual")
  Pi <- .kvmse_rotation(d, seed)
  base <- .kvmse_codebook(b - 1L)
  g <- .t1_lcg(seed + 1)
  S <- matrix(0, d, d)
  for (i in seq_len(d)) for (j in seq_len(d)) S[i, j] <- g$norm()
  rec <- matrix(0, n, d); mse <- rn <- numeric(n)
  for (i in seq_len(n)) {
    x <- V[i, ]
    y <- as.numeric(Pi %*% x)
    nrm <- sqrt(sum(x^2))
    sc <- if (nrm > 0) nrm / sqrt(d) else 1
    cb <- sc * base
    yt <- cb[.kvmse_quantize(y, cb) + 1L]
    xm <- as.numeric(t(Pi) %*% yt)
    r <- x - xm
    gam <- sqrt(sum(r^2))
    q <- ifelse(as.numeric(S %*% r) >= 0, 1, -1)
    k <- sqrt(pi / 2) / d * gam
    xt <- xm + as.numeric(k * (t(S) %*% q))
    rec[i, ] <- xt
    mse[i] <- sum((x - xt)^2)
    rn[i] <- gam
  }
  .t1_result(reconstruction = rec, mse = mse, residual_norm = rn,
             mean_mse = mean(mse), n = as.numeric(n), d = as.numeric(d),
             b = as.numeric(b),
             method = "TurboQuant_prod on a value cache, arXiv:2504.19874")
}
