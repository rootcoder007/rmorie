# SPDX-License-Identifier: AGPL-3.0-or-later
# Weibull survival with shared gamma frailty, and the SPDE precision on a grid.
# Identical to the Python arm morie.fn.spsurv.

#' Spatial survival and SPDE fields: Weibull gamma-frailty model and SPDE (Matern) GMRF precision
#'
#' \code{WeibullFrailtyFit}: hazard \code{lambda rho t^(rho - 1) exp(t(x) beta)}
#' with right censoring (likelihood \code{h^delta S}), and with
#' \code{cluster} a mean-one gamma frailty of variance \code{theta} integrated
#' out within clusters; L-BFGS on the log parameters with the analytic score,
#' then Newton polishing.
#' \code{SpdePrecisionGrid}: \code{Q = tau^2 (kappa^4 C + 2 kappa^2 G + G C^(-1) G)}
#' with lumped mass \code{h^2 I} and the Neumann 5-point stiffness.
#'
#' @param time,event Survival times and event indicators.
#' @param X Covariate matrix (no intercept).
#' @param cluster Cluster labels, or NULL for no frailty.
#' @param max_iter L-BFGS iterations.
#' @param nx,ny Grid size.
#' @param kappa,tau SPDE parameters.
#' @param h Grid spacing.
#' @return A list.
#' @references Lawson, A. B. (2021). Using R for Bayesian Spatial and
#'   Spatio-Temporal Health Modeling, ch. 15. Duchateau, L. and Janssen, P.
#'   (2008). The Frailty Model. Lindgren, F., Rue, H. and Lindstrom, J. (2011).
#'   JRSS B 73, 423-498.
#' @examples
#' SpdePrecisionGrid(2, 1, 1, 1)$Q
#' @export
WeibullFrailtyFit <- function(time, event, X, cluster = NULL, max_iter = 500) {
  ts <- as.numeric(time)
  dl <- as.numeric(event)
  Xm <- unname(as.matrix(X)) * 1
  n <- length(ts)
  p <- ncol(Xm)
  lt <- log(ts)
  groups <- if (is.null(cluster)) NULL else lapply(sort(unique(cluster)), function(g) which(cluster == g))
  dg <- function(x) {
    r <- 0
    while (x < 10) {
      r <- r - 1 / x
      x <- x + 1
    }
    f <- 1 / (x * x)
    r + log(x) - 0.5 / x - f * (1 / 12 - f * (1 / 120 - f * (1 / 252 - f * (1 / 240 - f / 132))))
  }
  fg <- function(par) {
    la <- par[1]
    lr <- par[2]
    beta <- par[2 + seq_len(p)]
    rho <- exp(lr)
    eta <- as.numeric(Xm %*% beta)
    Hi <- exp(la) * ts^rho * exp(eta)
    f <- sum(dl * (la + lr + (rho - 1) * lt + eta))
    g <- c(sum(dl), sum(dl * (1 + rho * lt)), as.numeric(crossprod(Xm, dl)))
    if (is.null(groups)) {
      f <- f - sum(Hi)
      g <- g - c(sum(Hi), sum(Hi * rho * lt), as.numeric(crossprod(Xm, Hi)))
      return(list(f = -f, g = -g))
    }
    th <- exp(par[3 + p])
    a <- 1 / th
    gth <- 0
    for (idx in groups) {
      D <- sum(dl[idx])
      H <- sum(Hi[idx])
      f <- f + lgamma(a + D) - lgamma(a) + D * log(th) - (a + D) * log(1 + th * H)
      cc <- (a + D) * th / (1 + th * H)
      g <- g - cc * c(H, sum(Hi[idx] * rho * lt[idx]), as.numeric(crossprod(Xm[idx, , drop = FALSE], Hi[idx])))
      gth <- gth - a * (dg(a + D) - dg(a)) + D + a * log(1 + th * H) - (a + D) * th * H / (1 + th * H)
    }
    list(f = -f, g = -c(g, gth))
  }
  k <- 2 + p + (if (is.null(groups)) 0 else 1)
  x0 <- c(log(sum(dl) / sum(ts)), 0, rep(0, p), if (!is.null(groups)) log(0.5))
  res <- LbfgsbMinimize(function(v) fg(v)$f, x0, grad = function(v) fg(v)$g, pgtol = 1e-10, factr = 10, max_iter = max_iter)
  x <- as.numeric(res$x)
  hess <- function(x) {
    H <- matrix(0, k, k)
    for (a in seq_len(k)) {
      h <- 1e-5 * max(1, abs(x[a]))
      up <- x
      dn <- x
      up[a] <- up[a] + h
      dn[a] <- dn[a] - h
      H[a, ] <- (fg(up)$g - fg(dn)$g) / (2 * h)
    }
    (H + t(H)) / 2
  }
  for (it in seq_len(20)) {
    step <- as.numeric(solve(hess(x), fg(x)$g))
    nw <- x - step
    if (fg(nw)$f > fg(x)$f + 1e-12 * abs(fg(x)$f)) break
    done <- max(abs(step) / pmax(1, abs(x))) <= 1e-13
    x <- nw
    if (done) break
  }
  H <- hess(x)
  V <- solve(H)
  out <- list(lambda_ = exp(x[1]), rho = exp(x[2]), beta = x[2 + seq_len(p)], loglik = -fg(x)$f,
              se_log_params = sqrt(pmax(diag(V), 0)))
  if (!is.null(groups)) out$theta <- exp(x[3 + p])
  out
}

#' @rdname WeibullFrailtyFit
#' @export
SpdePrecisionGrid <- function(nx, ny, kappa, tau, h = 1) {
  n <- nx * ny
  G <- matrix(0, n, n)
  for (j in 0:(ny - 1)) for (i in 0:(nx - 1)) {
    a <- i + nx * j + 1
    for (d in list(c(1, 0), c(-1, 0), c(0, 1), c(0, -1))) {
      ii <- i + d[1]
      jj <- j + d[2]
      if (ii >= 0 && ii < nx && jj >= 0 && jj < ny) {
        G[a, ii + nx * jj + 1] <- -1
        G[a, a] <- G[a, a] + 1
      }
    }
  }
  cc <- h * h
  Q <- tau^2 * (kappa^4 * cc * diag(n) + 2 * kappa^2 * G + G %*% G / cc)
  list(Q = Q, range = sqrt(8) / kappa, marginal_variance = 1 / (4 * pi * kappa^2 * tau^2))
}
