# SPDX-License-Identifier: AGPL-3.0-or-later
#' CMA-ES, the covariance matrix adaptation evolution strategy
#'
#' Update equations and default strategy parameters follow the reference
#' listing purecmaes.m (Hansen, arXiv:1604.00772), which annotates each line
#' with its equation number in Hansen and Ostermeier (2001), Completely
#' derandomized self-adaptation in evolution strategies, Evolutionary
#' Computation 9(2), 159-195.  Two departures make the run reproducible: the
#' mutation vectors are supplied by the caller, and the symmetric root
#' C^(1/2) = B D B-transpose is used in place of the factor B D, since B alone
#' is not unique (arbitrary eigenvector signs, and an arbitrary eigenbasis
#' whenever C has repeated eigenvalues, as it does at start-up).
#'
#' @param f objective to minimise, taking a length-N numeric vector.
#' @param x0 initial distribution mean.
#' @param sigma initial step size.
#' @param Z matrix of standard normal mutation vectors, iters*lam rows.
#' @param lam population size; default 4 + floor(3 log N).
#' @param iters number of generations.
#' @return list: estimate, fbest, xbest, xmean, sigma, C, evals, generations,
#'   n, method.
#' @keywords internal
#' @examples
#' Z <- matrix(0, 12, 2)
#' cmaopt(function(v) sum(v^2), c(0, 0), 0.5, Z, lam = 4, iters = 3)$fbest
#' @export
cmaopt <- function(f, x0, sigma = 0.5, Z = NULL, lam = NULL, iters = 10L) {
  xmean <- as.numeric(x0)
  N <- length(xmean)
  if (is.null(lam)) lam <- 4 + floor(3 * log(N))
  lam <- as.integer(lam)
  mu <- as.integer(lam %/% 2)
  wraw <- log(mu + 0.5) - log(seq_len(mu))
  w <- wraw / sum(wraw)
  mueff <- 1 / sum(w^2)
  Nf <- as.numeric(N)
  cc <- (4 + mueff / Nf) / (Nf + 4 + 2 * mueff / Nf)
  cs <- (mueff + 2) / (Nf + mueff + 5)
  c1 <- 2 / ((Nf + 1.3)^2 + mueff)
  cmu <- min(1 - c1, 2 * (mueff - 2 + 1 / mueff) / ((Nf + 2)^2 + mueff))
  damps <- 1 + 2 * max(0, sqrt((mueff - 1) / (Nf + 1)) - 1) + cs
  chiN <- Nf^0.5 * (1 - 1 / (4 * Nf) + 1 / (21 * Nf^2))
  pc <- rep(0, N)
  ps <- rep(0, N)
  C <- diag(N)
  sig <- as.numeric(sigma)
  Zm <- as.matrix(Z)
  fbest <- Inf
  xbest <- xmean
  counteval <- 0L
  symroots <- function(C) {
    ev <- eigen(C, symmetric = TRUE)
    lamv <- pmax(0, ev$values)
    half <- sqrt(lamv)
    ihalf <- ifelse(half > 0, 1 / half, 0)
    V <- ev$vectors
    list(root = V %*% diag(half, N) %*% t(V),
         iroot = V %*% diag(ihalf, N) %*% t(V))
  }
  for (g in seq_len(as.integer(iters))) {
    rr <- symroots(C)
    arz <- matrix(0, lam, N)
    ary <- matrix(0, lam, N)
    arx <- matrix(0, lam, N)
    fit <- numeric(lam)
    for (k in seq_len(lam)) {
      z <- Zm[(g - 1) * lam + k, ]
      y <- as.numeric(rr$root %*% z)
      x <- xmean + sig * y
      arz[k, ] <- z
      ary[k, ] <- y
      arx[k, ] <- x
      fv <- as.numeric(f(x))
      fit[k] <- fv
      counteval <- counteval + 1L
      if (fv < fbest) { fbest <- fv
      xbest <- x }
    }
    ord <- order(fit, seq_len(lam))
    sel <- ord[seq_len(mu)]
    xold <- xmean
    xmean <- as.numeric(t(arx[sel, , drop = FALSE]) %*% w)
    delta <- (xmean - xold) / sig
    cinvd <- as.numeric(rr$iroot %*% delta)
    ps <- (1 - cs) * ps + sqrt(cs * (2 - cs) * mueff) * cinvd
    psn <- sqrt(sum(ps^2))
    thresh <- sqrt(1 - (1 - cs)^(2 * counteval / lam))
    hsig <- as.numeric(psn / thresh / chiN < 1.4 + 2 / (Nf + 1))
    pc <- (1 - cc) * pc + hsig * sqrt(cc * (2 - cc) * mueff) * delta
    Cn <- (1 - c1 - cmu) * C + c1 * (outer(pc, pc) + (1 - hsig) * cc * (2 - cc) * C)
    for (i in seq_len(mu)) {
      y <- ary[sel[i], ]
      Cn <- Cn + cmu * w[i] * outer(y, y)
    }
    Cn <- (Cn + t(Cn)) / 2
    C <- Cn
    sig <- sig * exp((cs / damps) * (psn / chiN - 1))
  }
  list(estimate = as.numeric(fbest), fbest = as.numeric(fbest), xbest = xbest,
       xmean = xmean, sigma = as.numeric(sig), C = C,
       evals = as.integer(counteval), generations = as.integer(iters),
       n = as.integer(N), method = "CMA-ES (Hansen & Ostermeier 2001)")
}

# CANONICAL TEST
# Z <- matrix(0, 12, 2)
# r <- cmaopt(function(v) sum(v^2), c(0, 0), 0.5, Z, lam = 4, iters = 3)
# stopifnot(abs(r$fbest) < 1e-30, r$evals == 12L)

#' @rdname cmaopt
#' @keywords internal
#' @export
morie_cma_es <- cmaopt
