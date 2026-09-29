# SPDX-License-Identifier: AGPL-3.0-or-later
# Geostatistical simulation on the Philox stream.
# Identical to the Python arm morie.fn.geosim2.

#' Geostatistical simulation: SGS with block data, ensembles, p-field, LMC and collocated co-simulation, SIS, SNESIM, annealing
#'
#' \code{SgsBlockSimulate}: sequential Gaussian simulation (simple kriging
#' with known mean from the k nearest informed nodes plus all block-average
#' data with discretised point-to-block covariances). \code{ConditionalEnsemble}:
#' E-type mean, conditional variance and quantiles over realisations with
#' seeds \code{seed + r}. \code{PfieldSimulate}: \code{m + s Y} with an
#' unconditional correlated Gaussian field \code{Y}. \code{LmcConditionalSimulate}:
#' exact conditional joint simulation of p coregionalised fields.
#' \code{CollocatedCosimulate}: sequential Gaussian co-simulation by
#' collocated cokriging (Markov model 1). \code{SisMarkovBayes}: sequential
#' indicator simulation with optional collocated Markov-Bayes soft data.
#' \code{SnesimSimulate}: SNESIM by scanning the training image for the
#' data event. \code{AnnealingSimulate}: value swaps to a target variogram.
#' Covariance models are lists (model "Exp", "Sph" or "Gau"; sill, range,
#' nugget); Philox streams, paths and results match the Python arm.
#'
#' @param data_coords Data coordinates (matrix rows or list).
#' @param data_values Data values (for \code{LmcConditionalSimulate} a matrix, NA missing).
#' @param targets Target coordinates.
#' @param model Covariance model list.
#' @param mean Known mean.
#' @param k Neighbourhood size.
#' @param blocks List of list(points, value) block data, or NULL.
#' @param seed Philox seed.
#' @param n_real Number of realisations.
#' @param probs Quantile levels.
#' @param krige_mean,krige_sd Local kriging means and standard deviations.
#' @param coords Node coordinates.
#' @param components List of list(B, model) LMC components.
#' @param means Variable means, or NULL.
#' @param secondary Secondary values at the targets.
#' @param rho Primary-secondary correlation.
#' @param data_categories Data categories (0-based).
#' @param proportions Category proportions.
#' @param models Indicator covariance models per category.
#' @param soft Soft probabilities at the targets (rows), or NULL.
#' @param B Markov-Bayes calibration coefficients per category.
#' @param training Training image (matrix rows are y, columns x).
#' @param nx,ny Grid size.
#' @param template List of c(di, dj) offsets.
#' @param conditioning Data frame or matrix with columns i, j, value (0-based), or NULL.
#' @param values Values to place on the grid.
#' @param lags Integer lags.
#' @param target Target semivariogram values.
#' @param n_iter,t0,cooling,every Annealing iterations, start temperature, cooling factor and period.
#' @return A list.
#' @references Deutsch, C. V. and Journel, A. G. (1998). GSLIB, 2nd ed.
#'   Liu, Y. and Journel, A. G. (2009). Computers and Geosciences 35, 527-547.
#'   Goovaerts, P. (1997). Geostatistics for Natural Resources Evaluation.
#'   Froidevaux, R. (1993). In Geostatistics Troia 92, 73-84. Almeida, A. S. and
#'   Journel, A. G. (1994). Math. Geology 26, 565-588. Zhu, H. and Journel, A. G.
#'   (1993). In Geostatistics Troia 92. Strebelle, S. (2002). Math. Geology 34,
#'   1-21. Deutsch, C. V. and Cowan, P. W. (1996). In GSLIB, section V.6.
#' @examples
#' m <- list(model = "Exp", sill = 1, range = 2)
#' SgsBlockSimulate(rbind(c(0, 0)), 1.5, rbind(c(0, 0), c(5, 0)), m, seed = 1)$simulated
#' @export
SgsBlockSimulate <- function(data_coords, data_values, targets, model, mean = 0, k = 12, blocks = NULL, seed = 0) {
  kc <- .gs2_pts(data_coords)
  kv <- as.numeric(data_values)
  Tt <- .gs2_pts(targets)
  nt <- nrow(Tt)
  path <- .gs2_perm(nt, seed, 0)
  zn <- if (nt) .morie_random_normal(nt, seed = seed, stream = 1) else numeric(0)
  bpts <- lapply(blocks, function(b) .gs2_pts(b[[1]]))
  bval <- vapply(blocks, function(b) as.numeric(b[[2]]), 0)
  nb <- length(blocks)
  BB <- matrix(0, nb, nb)
  for (a in seq_len(nb)) for (b in seq_len(nb)) BB[a, b] <- mean(.gs2_covm(bpts[[a]], bpts[[b]], model))
  out <- numeric(nt)
  for (step in seq_len(nt)) {
    t <- path[step] + 1
    x <- Tt[t, ]
    ex <- which(kc[, 1] == x[1] & kc[, 2] == x[2])
    if (length(ex)) {
      out[t] <- kv[ex[1]]
      next
    }
    d <- sqrt((kc[, 1] - x[1])^2 + (kc[, 2] - x[2])^2)
    o <- order(d, seq_along(d))[seq_len(min(k, length(d)))]
    pts <- kc[o, , drop = FALSE]
    n1 <- nrow(pts)
    m <- n1 + nb
    C <- matrix(0, m, m)
    c0 <- numeric(m)
    if (n1) {
      C[seq_len(n1), seq_len(n1)] <- .gs2_covm(pts, pts, model)
      c0[seq_len(n1)] <- .gs2_covm(pts, matrix(x, 1), model)
    }
    for (b in seq_len(nb)) {
      v <- rowMeans(.gs2_covm(pts, bpts[[b]], model))
      C[seq_len(n1), n1 + b] <- v
      C[n1 + b, seq_len(n1)] <- v
      c0[n1 + b] <- mean(.gs2_covm(bpts[[b]], matrix(x, 1), model))
    }
    if (nb) C[n1 + seq_len(nb), n1 + seq_len(nb)] <- BB
    resid <- c(kv[o] - mean, bval - mean)
    c00 <- model$sill + (if (is.null(model$nugget)) 0 else model$nugget)
    if (m) {
      lam <- solve(C, c0)
      mu <- mean + sum(lam * resid)
      vr <- max(c00 - sum(lam * c0), 0)
    } else {
      mu <- mean
      vr <- c00
    }
    out[t] <- mu + sqrt(vr) * zn[step]
    kc <- rbind(kc, x)
    kv <- c(kv, out[t])
  }
  blk <- vapply(bpts, function(b) {
    idx <- which(vapply(seq_len(nt), function(q) any(b[, 1] == Tt[q, 1] & b[, 2] == Tt[q, 2]), TRUE))
    if (length(idx)) mean(out[idx]) else NaN
  }, 0)
  list(simulated = out, path = path, block_means = blk)
}

.gs2_pts <- function(x) {
  if (is.null(x) || length(x) == 0) return(matrix(0, 0, 2))
  if (is.list(x) && !is.data.frame(x)) return(do.call(rbind, lapply(x, as.numeric)))
  unname(as.matrix(x)) * 1
}

.gs2_rho <- function(h, model) {
  a <- model$range
  kind <- if (is.null(model$model)) "Exp" else model$model
  if (kind == "Sph") return(ifelse(h < a, 1 - 1.5 * h / a + 0.5 * (h / a)^3, 0))
  if (kind == "Gau") return(exp(-(h / a)^2))
  exp(-h / a)
}

.gs2_covm <- function(P, Q, model) {
  H <- sqrt(outer(P[, 1], Q[, 1], "-")^2 + outer(P[, 2], Q[, 2], "-")^2)
  nug <- if (is.null(model$nugget)) 0 else model$nugget
  C <- model$sill * .gs2_rho(H, model)
  C[H == 0] <- model$sill + nug
  matrix(C, nrow(P), nrow(Q))
}

.gs2_perm <- function(n, seed, stream) {
  perm <- seq_len(n) - 1
  if (n > 1) {
    u <- .morie_random_uniform(n, seed = seed, stream = stream)
    for (i in (n - 1):1) {
      j <- floor(u[i + 1] * (i + 1))
      tmp <- perm[i + 1]
      perm[i + 1] <- perm[j + 1]
      perm[j + 1] <- tmp
    }
  }
  perm
}

.gs2_chol <- function(C) {
  n <- nrow(C)
  L <- matrix(0, n, n)
  for (i in seq_len(n)) for (j in seq_len(i)) {
    s <- C[i, j] - sum(L[i, seq_len(j - 1)] * L[j, seq_len(j - 1)])
    L[i, j] <- if (i == j) sqrt(max(s, 0)) else if (L[j, j] > 0) s / L[j, j] else 0
  }
  L
}

.gs2_q7 <- function(s, p) {
  h <- (length(s) - 1) * p
  lo <- floor(h)
  hi <- min(lo + 1, length(s) - 1)
  w <- h - lo
  if (w > 0) (1 - w) * s[lo + 1] + w * s[hi + 1] else s[lo + 1]
}

#' @rdname SgsBlockSimulate
#' @export
ConditionalEnsemble <- function(data_coords, data_values, targets, model, mean = 0, k = 12, n_real = 20, probs = c(0.1, 0.5, 0.9),
                                seed = 0) {
  R <- t(vapply(seq_len(n_real) - 1, function(r) SgsBlockSimulate(data_coords, data_values, targets, model, mean = mean, k = k,
                                                                    seed = seed + r)$simulated, numeric(nrow(.gs2_pts(targets)))))
  R <- matrix(R, n_real)
  et <- colSums(R) / n_real
  vr <- colSums(sweep(R, 2, et)^2) / n_real
  qs <- lapply(probs, function(p) apply(R, 2, function(v) .gs2_q7(sort(v), p)))
  list(etype = et, variance = vr, quantiles = qs, realisations = R)
}

#' @rdname SgsBlockSimulate
#' @export
PfieldSimulate <- function(krige_mean, krige_sd, coords, model, seed = 0) {
  P <- .gs2_pts(coords)
  unit <- model
  unit$sill <- 1
  unit$nugget <- 0
  L <- .gs2_chol(.gs2_covm(P, P, unit))
  as.numeric(krige_mean) + as.numeric(krige_sd) * as.numeric(L %*% .morie_random_normal(nrow(P), seed = seed, stream = 0))
}

#' @rdname SgsBlockSimulate
#' @export
LmcConditionalSimulate <- function(data_coords, data_values, targets, components, means = NULL, seed = 0) {
  p <- nrow(components[[1]][[1]])
  mu <- if (is.null(means)) numeric(p) else as.numeric(means)
  Dc <- .gs2_pts(data_coords)
  Dv <- as.matrix(data_values)
  Tt <- .gs2_pts(targets)
  obs <- which(!is.na(Dv), arr.ind = TRUE)
  obs <- obs[order(obs[, 1], obs[, 2]), , drop = FALSE]
  Dl <- cbind(Dc[obs[, 1], , drop = FALSE], obs[, 2] - 1)
  z <- Dv[obs]
  tl <- cbind(Tt[rep(seq_len(nrow(Tt)), each = p), , drop = FALSE], rep(0:(p - 1), nrow(Tt)))
  known_idx <- vapply(seq_len(nrow(tl)), function(q) {
    w <- which(Dl[, 1] == tl[q, 1] & Dl[, 2] == tl[q, 2] & Dl[, 3] == tl[q, 3])
    if (length(w)) w[1] else 0L
  }, 0L)
  free <- tl[known_idx == 0, , drop = FALSE]
  cc <- function(A, B) {
    H <- sqrt(outer(A[, 1], B[, 1], "-")^2 + outer(A[, 2], B[, 2], "-")^2)
    out <- matrix(0, nrow(A), nrow(B))
    for (comp in components) {
      R <- .gs2_rho(H, comp[[2]])
      R[H == 0] <- 1
      out <- out + comp[[1]][cbind(rep(A[, 3] + 1, nrow(B)), rep(B[, 3] + 1, each = nrow(A)))] * R
    }
    out
  }
  nd <- nrow(Dl)
  if (nd) {
    Sdd <- cc(Dl, Dl)
    Std <- cc(free, Dl)
    cmu <- mu[free[, 3] + 1] + as.numeric(Std %*% solve(Sdd, z - mu[Dl[, 3] + 1]))
    S <- cc(free, free) - Std %*% solve(Sdd, t(Std))
  } else {
    cmu <- mu[free[, 3] + 1]
    S <- cc(free, free)
  }
  L <- .gs2_chol(S)
  sim <- cmu + as.numeric(L %*% if (nrow(free)) .morie_random_normal(nrow(free), seed = seed, stream = 0) else numeric(0))
  vals <- numeric(nrow(tl))
  vals[known_idx > 0] <- z[known_idx[known_idx > 0]]
  vals[known_idx == 0] <- sim
  list(simulated = matrix(vals, nrow(Tt), p, byrow = TRUE))
}

#' @rdname SgsBlockSimulate
#' @export
CollocatedCosimulate <- function(data_coords, data_values, targets, secondary, model, rho, k = 12, seed = 0) {
  unit <- model
  unit$sill <- 1
  unit$nugget <- 0
  kc <- .gs2_pts(data_coords)
  kv <- as.numeric(data_values)
  Tt <- .gs2_pts(targets)
  nt <- nrow(Tt)
  path <- .gs2_perm(nt, seed, 0)
  zn <- .morie_random_normal(nt, seed = seed, stream = 1)
  out <- numeric(nt)
  for (step in seq_len(nt)) {
    t <- path[step] + 1
    x <- Tt[t, ]
    ex <- which(kc[, 1] == x[1] & kc[, 2] == x[2])
    if (length(ex)) {
      out[t] <- kv[ex[1]]
      next
    }
    d <- sqrt((kc[, 1] - x[1])^2 + (kc[, 2] - x[2])^2)
    o <- order(d, seq_along(d))[seq_len(min(k, length(d)))]
    pts <- kc[o, , drop = FALSE]
    n1 <- nrow(pts)
    cx <- as.numeric(.gs2_covm(pts, matrix(x, 1), unit))
    C <- rbind(cbind(.gs2_covm(pts, pts, unit), rho * cx), c(rho * cx, 1))
    c0 <- c(cx, rho)
    lam <- solve(C, c0)
    mu <- sum(lam[seq_len(n1)] * kv[o]) + lam[n1 + 1] * secondary[t]
    out[t] <- mu + sqrt(max(1 - sum(lam * c0), 0)) * zn[step]
    kc <- rbind(kc, x)
    kv <- c(kv, out[t])
  }
  list(simulated = out, path = path)
}

#' @rdname SgsBlockSimulate
#' @export
SisMarkovBayes <- function(data_coords, data_categories, targets, proportions, models, k = 12, soft = NULL, B = NULL, seed = 0) {
  K <- length(proportions)
  kc <- .gs2_pts(data_coords)
  kv <- as.integer(data_categories)
  Tt <- .gs2_pts(targets)
  nt <- nrow(Tt)
  path <- .gs2_perm(nt, seed, 0)
  u <- .morie_random_uniform(nt, seed = seed, stream = 1)
  out <- integer(nt)
  probs <- vector("list", nt)
  for (step in seq_len(nt)) {
    t <- path[step] + 1
    x <- Tt[t, ]
    ex <- which(kc[, 1] == x[1] & kc[, 2] == x[2])
    if (length(ex)) {
      out[t] <- kv[ex[1]]
      probs[[t]] <- as.numeric(seq_len(K) - 1 == kv[ex[1]])
      next
    }
    d <- sqrt((kc[, 1] - x[1])^2 + (kc[, 2] - x[2])^2)
    o <- order(d, seq_along(d))[seq_len(min(k, length(d)))]
    pts <- kc[o, , drop = FALSE]
    cats <- kv[o]
    pr <- vapply(seq_len(K), function(c) {
      m <- models[[c]]
      C <- .gs2_covm(pts, pts, m)
      c0 <- as.numeric(.gs2_covm(pts, matrix(x, 1), m))
      res <- as.numeric(cats == c - 1) - proportions[c]
      if (!is.null(soft)) {
        bc <- B[c]
        cxx <- .gs2_covm(matrix(x, 1), matrix(x, 1), m)[1, 1]
        C <- rbind(cbind(C, bc * c0), c(bc * c0, bc * cxx))
        c0 <- c(c0, bc * cxx)
        res <- c(res, soft[t, c] - proportions[c])
      }
      lam <- if (length(c0)) solve(C, c0) else numeric(0)
      min(max(proportions[c] + sum(lam * res), 0), 1)
    }, 0)
    pr <- if (sum(pr) > 0) pr / sum(pr) else as.numeric(proportions)
    cat_ <- which(u[step] <= cumsum(pr))[1]
    if (is.na(cat_)) cat_ <- K
    out[t] <- cat_ - 1L
    probs[[t]] <- pr
    kc <- rbind(kc, x)
    kv <- c(kv, cat_ - 1L)
  }
  list(simulated = out, probabilities = probs, path = path)
}

#' @rdname SgsBlockSimulate
#' @export
SnesimSimulate <- function(training, nx, ny, template, conditioning = NULL, seed = 0) {
  TI <- as.matrix(training)
  TJ <- nrow(TI)
  TIw <- ncol(TI)
  cats <- sort(unique(as.numeric(TI)))
  grid <- matrix(NA_real_, ny, nx)
  if (!is.null(conditioning)) for (q in seq_len(nrow(conditioning))) grid[conditioning[q, 2] + 1, conditioning[q, 1] + 1] <- conditioning[q, 3]
  nodes <- which(is.na(t(grid)))
  ni <- (nodes - 1) %% nx
  nj <- (nodes - 1) %/% nx
  path <- .gs2_perm(length(nodes), seed, 0)
  u <- .morie_random_uniform(length(nodes), seed = seed, stream = 1)
  marg <- vapply(cats, function(c) sum(TI == c) / length(TI), 0)
  for (step in seq_along(nodes)) {
    q <- path[step] + 1
    i <- ni[q]
    j <- nj[q]
    ev <- matrix(0, 0, 3)
    for (tp in template) {
      a <- i + tp[1]
      b <- j + tp[2]
      if (a >= 0 && a < nx && b >= 0 && b < ny && !is.na(grid[b + 1, a + 1])) ev <- rbind(ev, c(tp[1], tp[2], grid[b + 1, a + 1]))
    }
    if (nrow(ev)) ev <- ev[order(ev[, 1]^2 + ev[, 2]^2, ev[, 1], ev[, 2]), , drop = FALSE]
    pr <- marg
    while (nrow(ev)) {
      counts <- numeric(length(cats))
      for (tj in 0:(TJ - 1)) for (ti in 0:(TIw - 1)) {
        a <- ti + ev[, 1]
        b <- tj + ev[, 2]
        if (all(a >= 0 & a < TIw & b >= 0 & b < TJ) && all(TI[cbind(b + 1, a + 1)] == ev[, 3])) {
          w <- which(cats == TI[tj + 1, ti + 1])
          counts[w] <- counts[w] + 1
        }
      }
      if (sum(counts) > 0) {
        pr <- counts / sum(counts)
        break
      }
      ev <- ev[-nrow(ev), , drop = FALSE]
    }
    pick <- which(u[step] <= cumsum(pr))[1]
    if (is.na(pick)) pick <- length(cats)
    grid[j + 1, i + 1] <- cats[pick]
  }
  list(grid = grid, categories = cats)
}

#' @rdname SgsBlockSimulate
#' @export
AnnealingSimulate <- function(values, nx, ny, lags, target, n_iter = 5000, t0 = 1, cooling = 0.9, every = 100, seed = 0) {
  n <- nx * ny
  perm <- .gs2_perm(n, seed, 0)
  g <- matrix(0, ny, nx)
  for (idx in 0:(n - 1)) g[idx %/% nx + 1, idx %% nx + 1] <- values[perm[idx + 1] + 1]
  L <- length(lags)
  npair <- ny * (nx - lags) + nx * (ny - lags)
  sums <- function() vapply(lags, function(h) {
    s <- 0
    if (h < nx) s <- s + sum((g[, seq_len(nx - h)] - g[, h + seq_len(nx - h)])^2)
    if (h < ny) s <- s + sum((g[seq_len(ny - h), ] - g[h + seq_len(ny - h), ])^2)
    s
  }, 0)
  objective <- function(s) sum(((s / (2 * npair)) - target)^2 / target^2)
  contrib <- function(i, j) vapply(lags, function(h) {
    c <- 0
    for (d in list(c(h, 0), c(-h, 0), c(0, h), c(0, -h))) {
      ii <- i + d[1]
      jj <- j + d[2]
      if (ii >= 0 && ii < nx && jj >= 0 && jj < ny) c <- c + (g[j + 1, i + 1] - g[jj + 1, ii + 1])^2
    }
    c
  }, 0)
  S <- sums()
  O <- objective(S)
  Tp <- t0
  acc <- 0
  for (s in seq_len(n_iter) - 1) {
    u <- .morie_random_uniform(3, seed = seed, stream = s + 1)
    p1 <- min(floor(u[1] * n), n - 1)
    p2 <- min(floor(u[2] * n), n - 1)
    if (p1 != p2) {
      i1 <- p1 %% nx
      j1 <- p1 %/% nx
      i2 <- p2 %% nx
      j2 <- p2 %/% nx
      before <- contrib(i1, j1) + contrib(i2, j2)
      tmp <- g[j1 + 1, i1 + 1]
      g[j1 + 1, i1 + 1] <- g[j2 + 1, i2 + 1]
      g[j2 + 1, i2 + 1] <- tmp
      after <- contrib(i1, j1) + contrib(i2, j2)
      Sn <- S + after - before
      On <- objective(Sn)
      if (On <= O || u[3] < exp(-(On - O) / Tp)) {
        S <- Sn
        O <- On
        acc <- acc + 1
      } else {
        tmp <- g[j1 + 1, i1 + 1]
        g[j1 + 1, i1 + 1] <- g[j2 + 1, i2 + 1]
        g[j2 + 1, i2 + 1] <- tmp
      }
    }
    if ((s + 1) %% every == 0) Tp <- Tp * cooling
  }
  list(grid = g, objective = O, accepted = acc, variogram = S / (2 * npair))
}
