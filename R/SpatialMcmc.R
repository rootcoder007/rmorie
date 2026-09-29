# SPDX-License-Identifier: AGPL-3.0-or-later
# MCMC samplers for spatial posteriors on the Philox stream.
# Identical to the Python arm morie.fn.spmcmc.

#' Spatial MCMC: Poisson-CAR target, Metropolis-Hastings, HMC, NUTS and parallel tempering
#'
#' \code{CarPoissonTarget}: log posterior and gradient of
#' \code{y_i ~ Poisson(E_i exp(beta_0 + phi_i))},
#' \code{phi ~ N(0, (tau (D - rho A))^(-1))}, \code{beta_0 ~ N(0, prior_sd^2)}.
#' \code{MhSpatial}: random-walk Metropolis (joint or single-site), iteration
#' \code{t} on Philox streams \code{2t} (normals) and \code{2t + 1} (uniforms).
#' \code{HmcSpatial}: leapfrog Hamiltonian Monte Carlo. \code{NutsSpatial}:
#' efficient No-U-Turn sampler (Hoffman and Gelman 2014, Algorithm 3) with
#' uniforms drawn in order from a block on stream \code{2t + 1}.
#' \code{TemperedSpatial}: parallel tempering with alternating adjacent
#' swaps. Draws match the Python arm exactly.
#'
#' @param y,expected Counts and expected counts.
#' @param A Adjacency matrix.
#' @param tau,rho CAR precision and spatial dependence.
#' @param prior_sd Prior standard deviation of the intercept.
#' @param logp,grad Log-density and gradient functions.
#' @param x0 Starting value.
#' @param n_iter Number of iterations.
#' @param step Random-walk step.
#' @param single_site Update one coordinate at a time.
#' @param seed Philox seed.
#' @param eps Leapfrog step size.
#' @param n_leapfrog Leapfrog steps per iteration.
#' @param max_depth Maximum NUTS tree depth.
#' @param temps Temperatures (the first is the target).
#' @return A list (\code{CarPoissonTarget} returns the two functions).
#' @references Banerjee, S., Carlin, B. P. and Gelfand, A. E. (2014).
#'   Hierarchical Modeling and Analysis for Spatial Data. Hastings, W. K.
#'   (1970). Biometrika 57, 97-109. Neal, R. M. (2011). In Handbook of MCMC,
#'   ch. 5. Hoffman, M. D. and Gelman, A. (2014). JMLR 15, 1593-1623. Geyer,
#'   C. J. (1991). Computing Science and Statistics 23, 156-163.
#' @examples
#' tg <- CarPoissonTarget(c(1, 2), c(1, 1), rbind(c(0, 1), c(1, 0)))
#' tg$logp(c(0, 0, 0))
#' @export
CarPoissonTarget <- function(y, expected, A, tau = 1, rho = 0.9, prior_sd = 10) {
  y <- as.numeric(y)
  E <- as.numeric(expected)
  A <- unname(as.matrix(A)) * 1
  Q <- tau * (diag(rowSums(A)) - rho * A)
  logp <- function(x) {
    b <- x[1]
    phi <- x[-1]
    eta <- b + phi
    sum(y * eta - E * exp(eta)) - 0.5 * sum(phi * (Q %*% phi)) - 0.5 * b * b / prior_sd^2
  }
  grad <- function(x) {
    b <- x[1]
    phi <- x[-1]
    r <- y - E * exp(b + phi)
    c(sum(r) - b / prior_sd^2, r - as.numeric(Q %*% phi))
  }
  list(logp = logp, grad = grad)
}

#' @rdname CarPoissonTarget
#' @export
MhSpatial <- function(logp, x0, n_iter, step = 0.1, single_site = FALSE, seed = 0) {
  x <- as.numeric(x0)
  d <- length(x)
  lp <- logp(x)
  out <- matrix(0, n_iter, d)
  acc <- 0
  tot <- 0
  for (t in seq_len(n_iter) - 1) {
    z <- .morie_random_normal(d, seed = seed, stream = 2 * t)
    u <- .morie_random_uniform(if (single_site) d else 1, seed = seed, stream = 2 * t + 1)
    if (single_site) {
      for (j in seq_len(d)) {
        prop <- x
        prop[j] <- prop[j] + step * z[j]
        lq <- logp(prop)
        tot <- tot + 1
        if (log(u[j]) < lq - lp) {
          x <- prop
          lp <- lq
          acc <- acc + 1
        }
      }
    } else {
      prop <- x + step * z
      lq <- logp(prop)
      tot <- tot + 1
      if (log(u[1]) < lq - lp) {
        x <- prop
        lp <- lq
        acc <- acc + 1
      }
    }
    out[t + 1, ] <- x
  }
  list(samples = out, acceptance = acc / tot, logp = lp)
}

.smc_leapfrog <- function(grad, x, p, eps) {
  p <- p + 0.5 * eps * grad(x)
  x <- x + eps * p
  p <- p + 0.5 * eps * grad(x)
  list(x = x, p = p)
}

#' @rdname CarPoissonTarget
#' @export
HmcSpatial <- function(logp, grad, x0, n_iter, eps = 0.05, n_leapfrog = 20, seed = 0) {
  x <- as.numeric(x0)
  d <- length(x)
  lp <- logp(x)
  out <- matrix(0, n_iter, d)
  acc <- 0
  for (t in seq_len(n_iter) - 1) {
    p0 <- .morie_random_normal(d, seed = seed, stream = 2 * t)
    u <- .morie_random_uniform(1, seed = seed, stream = 2 * t + 1)
    s <- list(x = x, p = p0)
    for (l in seq_len(n_leapfrog)) s <- .smc_leapfrog(grad, s$x, s$p, eps)
    ln <- logp(s$x)
    if (log(u) < (-lp + 0.5 * sum(p0^2)) - (-ln + 0.5 * sum(s$p^2))) {
      x <- s$x
      lp <- ln
      acc <- acc + 1
    }
    out[t + 1, ] <- x
  }
  list(samples = out, acceptance = acc / n_iter, logp = lp)
}

#' @rdname CarPoissonTarget
#' @export
NutsSpatial <- function(logp, grad, x0, n_iter, eps = 0.05, max_depth = 8, seed = 0) {
  x <- as.numeric(x0)
  d <- length(x)
  out <- matrix(0, n_iter, d)
  depths <- integer(n_iter)
  nblock <- 2^max_depth + 2 * max_depth + 4
  env <- new.env()
  draw <- function() {
    env$ptr <- env$ptr + 1
    env$U[env$ptr]
  }
  build <- function(xx, pp, lu, v, jj) {
    if (jj == 0) {
      s <- .smc_leapfrog(grad, xx, pp, v * eps)
      h <- logp(s$x) - 0.5 * sum(s$p^2)
      return(list(xm = s$x, pm = s$p, xp = s$x, pp = s$p, x1 = s$x, n = as.numeric(lu <= h), s = as.numeric(lu < h + 1000)))
    }
    a <- build(xx, pp, lu, v, jj - 1)
    if (a$s == 1) {
      if (v == -1) {
        b <- build(a$xm, a$pm, lu, v, jj - 1)
        a$xm <- b$xm
        a$pm <- b$pm
      } else {
        b <- build(a$xp, a$pp, lu, v, jj - 1)
        a$xp <- b$xp
        a$pp <- b$pp
      }
      if (a$n + b$n > 0 && draw() < b$n / (a$n + b$n)) a$x1 <- b$x1
      dx <- a$xp - a$xm
      a$s <- b$s * as.numeric(sum(dx * a$pm) >= 0) * as.numeric(sum(dx * a$pp) >= 0)
      a$n <- a$n + b$n
    }
    a
  }
  for (t in seq_len(n_iter) - 1) {
    p0 <- .morie_random_normal(d, seed = seed, stream = 2 * t)
    env$U <- .morie_random_uniform(nblock, seed = seed, stream = 2 * t + 1)
    env$ptr <- 0
    logu <- logp(x) - 0.5 * sum(p0^2) + log(draw())
    xm <- xp <- x
    pm <- pp <- p0
    j <- 0
    n <- 1
    s <- 1
    xnew <- x
    while (s == 1 && j < max_depth) {
      v <- if (draw() < 0.5) -1 else 1
      if (v == -1) {
        b <- build(xm, pm, logu, v, j)
        xm <- b$xm
        pm <- b$pm
      } else {
        b <- build(xp, pp, logu, v, j)
        xp <- b$xp
        pp <- b$pp
      }
      if (b$s == 1 && draw() < min(1, b$n / n)) xnew <- b$x1
      n <- n + b$n
      dx <- xp - xm
      s <- b$s * as.numeric(sum(dx * pm) >= 0) * as.numeric(sum(dx * pp) >= 0)
      j <- j + 1
    }
    x <- xnew
    out[t + 1, ] <- x
    depths[t + 1] <- j
  }
  list(samples = out, depths = depths)
}

#' @rdname CarPoissonTarget
#' @export
TemperedSpatial <- function(logp, x0, n_iter, temps, step = 0.1, seed = 0) {
  C <- length(temps)
  xs <- rep(list(as.numeric(x0)), C)
  lps <- vapply(xs, logp, 0)
  d <- length(x0)
  out <- matrix(0, n_iter, d)
  sw_acc <- sw_try <- numeric(max(C - 1, 0))
  for (t in seq_len(n_iter) - 1) {
    base <- t * (2 * C + 1)
    for (c in seq_len(C)) {
      z <- .morie_random_normal(d, seed = seed, stream = base + 2 * (c - 1))
      u <- .morie_random_uniform(1, seed = seed, stream = base + 2 * (c - 1) + 1)
      prop <- xs[[c]] + step * sqrt(temps[c]) * z
      lq <- logp(prop)
      if (log(u) < (lq - lps[c]) / temps[c]) {
        xs[[c]] <- prop
        lps[c] <- lq
      }
    }
    if (C > 1) {
      k <- t %% (C - 1) + 1
      u <- .morie_random_uniform(1, seed = seed, stream = base + 2 * C)
      sw_try[k] <- sw_try[k] + 1
      if (log(u) < (1 / temps[k] - 1 / temps[k + 1]) * (lps[k + 1] - lps[k])) {
        tmp <- xs[[k]]
        xs[[k]] <- xs[[k + 1]]
        xs[[k + 1]] <- tmp
        tl <- lps[k]
        lps[k] <- lps[k + 1]
        lps[k + 1] <- tl
        sw_acc[k] <- sw_acc[k] + 1
      }
    }
    out[t + 1, ] <- xs[[1]]
  }
  list(samples = out, swap_rate = ifelse(sw_try > 0, sw_acc / sw_try, 0), states = xs)
}
