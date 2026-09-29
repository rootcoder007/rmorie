# SPDX-License-Identifier: AGPL-3.0-or-later
# Species distribution (MaxEnt) and k-LoCoH home ranges.
# Identical to the Python arm morie.fn.specdist.

#' Species distribution and home range: MaxEnt features, fit and prediction; k-LoCoH isopleths
#'
#' \code{MaxentFeatures}: linear, quadratic, product and hinge features
#' (scaled by the covariate ranges). \code{MaxentFit}: the Gibbs distribution
#' over background points (presences appended when \code{add_presence})
#' minimising \code{-mean_presence eta + log sum_b exp(eta) + (l2/2)||lambda||^2 + l1 ||lambda||_1},
#' by Newton steps (no lasso) or soft-thresholded coordinate descent.
#' \code{MaxentPredict}: raw, logistic and cloglog outputs.
#' \code{LocohHomeRange}: k-LoCoH isopleths, the union of the smallest local
#' hulls covering a fraction of the points, with exact union areas.
#'
#' @param X Covariate matrix.
#' @param classes Feature classes: l, q, p, h.
#' @param hinge_knots Number of hinge knots per covariate.
#' @param ranges List of (min, max) per covariate, or NULL.
#' @param presence,background Feature matrices of presence and background points.
#' @param l2,l1 Ridge and lasso penalties.
#' @param add_presence Append presences to the background.
#' @param max_iter,tol Iteration limit and tolerance.
#' @param fit Result of \code{MaxentFit}.
#' @param features Feature matrix to predict at.
#' @param output "raw", "logistic" or "cloglog".
#' @param points Location matrix (two columns).
#' @param k Neighbourhood size (point plus k - 1 neighbours).
#' @param levels Isopleth levels.
#' @return A list or numeric vector.
#' @references Phillips, S. J., Anderson, R. P. and Schapire, R. E. (2006).
#'   Ecol. Modelling 190, 231-259. Phillips, S. J. et al. (2017). Ecography
#'   40, 887-893. Dudik, M., Phillips, S. J. and Schapire, R. E. (2004). COLT
#'   2004, 472-486. Getz, W. M. and Wilmers, C. C. (2004). Ecography 27, 489-505.
#' @examples
#' f <- MaxentFit(rbind(1, 0.8), rbind(0, 0.5, 1), add_presence = FALSE)
#' MaxentPredict(f, rbind(0, 1), output = "raw")
#' @export
MaxentFeatures <- function(X, classes = "lq", hinge_knots = 5, ranges = NULL) {
  X <- unname(as.matrix(X)) * 1
  p <- ncol(X)
  rng <- if (is.null(ranges)) lapply(seq_len(p), function(j) c(min(X[, j]), max(X[, j]))) else ranges
  sc <- function(v, j) if (rng[[j]][2] > rng[[j]][1]) (v - rng[[j]][1]) / (rng[[j]][2] - rng[[j]][1]) else 0 * v
  cls <- strsplit(classes, "")[[1]]
  out <- list()
  nm <- list()
  if ("l" %in% cls) for (j in seq_len(p)) {
    out[[length(out) + 1]] <- sc(X[, j], j)
    nm[[length(nm) + 1]] <- list("l", j - 1)
  }
  if ("q" %in% cls) for (j in seq_len(p)) {
    out[[length(out) + 1]] <- sc(X[, j], j)^2
    nm[[length(nm) + 1]] <- list("q", j - 1)
  }
  if ("p" %in% cls && p > 1) for (j in 1:(p - 1)) for (k in (j + 1):p) {
    out[[length(out) + 1]] <- sc(X[, j], j) * sc(X[, k], k)
    nm[[length(nm) + 1]] <- list("p", j - 1, k - 1)
  }
  if ("h" %in% cls) for (j in seq_len(p)) {
    lo <- rng[[j]][1]
    hi <- rng[[j]][2]
    span <- if (hi > lo) hi - lo else 1
    for (m in seq_len(hinge_knots)) {
      kn <- lo + (hi - lo) * m / (hinge_knots + 1)
      d <- (X[, j] - kn) / span
      out[[length(out) + 1]] <- pmax(d, 0)
      out[[length(out) + 1]] <- pmax(-d, 0)
      nm[[length(nm) + 1]] <- list("hf", j - 1, kn)
      nm[[length(nm) + 1]] <- list("hr", j - 1, kn)
    }
  }
  list(features = do.call(cbind, out), names = nm, ranges = rng)
}

#' @rdname MaxentFeatures
#' @export
MaxentFit <- function(presence, background, l2 = 0, l1 = 0, add_presence = TRUE, max_iter = 500, tol = 1e-12) {
  P <- unname(as.matrix(presence)) * 1
  B <- unname(as.matrix(background)) * 1
  if (add_presence) B <- rbind(B, P)
  k <- ncol(P)
  target <- colSums(P) / nrow(P)
  dist_ <- function(lm) {
    eta <- as.numeric(B %*% lm)
    mx <- max(eta)
    w <- exp(eta - mx)
    z <- sum(w)
    list(q = w / z, lz = mx + log(z))
  }
  obj <- function(lm, lz) -sum(target * lm) + lz + 0.5 * l2 * sum(lm^2) + l1 * sum(abs(lm))
  lam <- numeric(k)
  d0 <- dist_(lam)
  q <- d0$q
  lz <- d0$lz
  f <- obj(lam, lz)
  it <- 0
  for (iter in seq_len(max_iter)) {
    it <- it + 1
    if (l1 == 0) {
      mu <- as.numeric(crossprod(B, q))
      g <- mu - target + l2 * lam
      if (max(abs(g)) <= tol) break
      Bc <- sweep(B, 2, mu)
      H <- crossprod(Bc, Bc * q) + diag(l2, k)
      d <- as.numeric(solve(H, -g))
      step <- 1
      repeat {
        nw <- lam + step * d
        dn <- dist_(nw)
        fn <- obj(nw, dn$lz)
        if (fn <= f + 1e-4 * step * sum(g * d) || step < 1e-10) break
        step <- step / 2
      }
      lam <- nw
      q <- dn$q
      lz <- dn$lz
      f <- fn
    } else {
      change <- 0
      for (j in seq_len(k)) {
        muj <- sum(q * B[, j])
        vr <- sum(q * (B[, j] - muj)^2)
        if (vr + l2 <= 0) next
        z <- lam[j] * vr - (muj - target[j])
        newj <- sign(z) * max(abs(z) - l1, 0) / (vr + l2)
        if (newj != lam[j]) {
          trial <- lam
          step <- 1
          repeat {
            trial[j] <- lam[j] + step * (newj - lam[j])
            dn <- dist_(trial)
            fn <- obj(trial, dn$lz)
            if (fn <= f + 1e-13 * abs(f) || step < 1e-10) break
            step <- step / 2
          }
          change <- max(change, abs(trial[j] - lam[j]))
          lam <- trial
          q <- dn$q
          lz <- dn$lz
          f <- fn
        }
      }
      if (change <= tol) break
    }
  }
  list(lambdas = lam, entropy = -sum(q[q > 0] * log(q[q > 0])), log_normaliser = lz, objective = f, iterations = it,
       background = B)
}

#' @rdname MaxentFeatures
#' @export
MaxentPredict <- function(fit, features, output = "cloglog") {
  q <- exp(as.numeric(as.matrix(features) %*% fit$lambdas) - fit$log_normaliser)
  H <- exp(fit$entropy)
  if (output == "raw") return(q)
  if (output == "logistic") return(H * q / (1 + H * q))
  1 - exp(-H * q)
}

.sd_hull <- function(P) {
  P <- unique(P)
  P <- P[order(P[, 1], P[, 2]), , drop = FALSE]
  if (nrow(P) <= 2) return(P)
  cross <- function(o, a, b) (a[1] - o[1]) * (b[2] - o[2]) - (a[2] - o[2]) * (b[1] - o[1])
  build <- function(idx) {
    h <- list()
    for (i in idx) {
      while (length(h) >= 2 && cross(h[[length(h) - 1]], h[[length(h)]], P[i, ]) <= 0) h[[length(h)]] <- NULL
      h[[length(h) + 1]] <- P[i, ]
    }
    h
  }
  lower <- build(seq_len(nrow(P)))
  upper <- build(rev(seq_len(nrow(P))))
  do.call(rbind, c(lower[-length(lower)], upper[-length(upper)]))
}

.sd_area <- function(h) {
  n <- nrow(h)
  j <- c(2:n, 1)
  abs(sum(h[, 1] * h[j, 2] - h[j, 1] * h[, 2])) / 2
}

.sd_inside <- function(h, p) {
  n <- nrow(h)
  if (n < 3) return(FALSE)
  j <- c(2:n, 1)
  all((h[j, 1] - h[, 1]) * (p[2] - h[, 2]) - (h[j, 2] - h[, 2]) * (p[1] - h[, 1]) >= -1e-12)
}

.sd_union_area <- function(polys) {
  polys <- Filter(function(p) nrow(p) >= 3, polys)
  if (!length(polys)) return(0)
  E <- do.call(rbind, lapply(polys, function(p) cbind(p, p[c(2:nrow(p), 1), , drop = FALSE])))
  xs <- unique(unlist(lapply(polys, function(p) p[, 1])))
  ne <- nrow(E)
  if (ne > 1) for (a in 1:(ne - 1)) for (b in (a + 1):ne) {
    x1 <- E[a, 1]
    y1 <- E[a, 2]
    x2 <- E[a, 3]
    y2 <- E[a, 4]
    x3 <- E[b, 1]
    y3 <- E[b, 2]
    x4 <- E[b, 3]
    y4 <- E[b, 4]
    den <- (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
    if (den == 0) next
    t <- ((x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)) / den
    u <- -((x1 - x2) * (y1 - y3) - (y1 - y2) * (x1 - x3)) / den
    if (t >= 0 && t <= 1 && u >= 0 && u <= 1) xs <- c(xs, x1 + t * (x2 - x1))
  }
  xs <- sort(unique(xs))
  total <- 0
  for (q in seq_len(length(xs) - 1)) {
    x0 <- xs[q]
    x1 <- xs[q + 1]
    if (x1 <= x0) next
    xm <- (x0 + x1) / 2
    ivs <- do.call(rbind, lapply(polys, function(p) {
      n <- nrow(p)
      j <- c(2:n, 1)
      hit <- (p[, 1] - xm) * (p[j, 1] - xm) < 0
      if (sum(hit) < 2) return(NULL)
      ys <- p[hit, 2] + (p[j, 2][hit] - p[hit, 2]) * (xm - p[hit, 1]) / (p[j, 1][hit] - p[hit, 1])
      c(min(ys), max(ys))
    }))
    if (is.null(ivs)) next
    ivs <- ivs[order(ivs[, 1], ivs[, 2]), , drop = FALSE]
    len <- 0
    lo <- ivs[1, 1]
    hi <- ivs[1, 2]
    for (r in seq_len(nrow(ivs))[-1]) {
      if (ivs[r, 1] > hi) {
        len <- len + hi - lo
        lo <- ivs[r, 1]
        hi <- ivs[r, 2]
      } else {
        hi <- max(hi, ivs[r, 2])
      }
    }
    total <- total + (len + hi - lo) * (x1 - x0)
  }
  total
}

#' @rdname MaxentFeatures
#' @export
LocohHomeRange <- function(points, k, levels = c(0.5, 0.95)) {
  pts <- matrix(as.numeric(unlist(points)), ncol = 2, byrow = is.list(points))
  n <- nrow(pts)
  hulls <- lapply(seq_len(n), function(i) {
    d <- sqrt((pts[, 1] - pts[i, 1])^2 + (pts[, 2] - pts[i, 2])^2)
    .sd_hull(pts[order(d, seq_len(n))[seq_len(k)], , drop = FALSE])
  })
  areas <- vapply(hulls, function(h) if (nrow(h) >= 3) .sd_area(h) else 0, 0)
  ord <- order(areas, seq_len(n))
  covered <- rep(FALSE, n)
  lv <- sort(levels)
  li <- 1
  out_a <- out_n <- cover <- numeric(0)
  used <- integer(0)
  for (s in seq_along(ord)) {
    h <- ord[s]
    used <- c(used, h)
    for (p in seq_len(n)) if (!covered[p] && .sd_inside(hulls[[h]], pts[p, ])) covered[p] <- TRUE
    frac <- sum(covered) / n
    while (li <= length(lv) && frac >= lv[li]) {
      out_a <- c(out_a, .sd_union_area(hulls[used]))
      out_n <- c(out_n, s)
      cover <- c(cover, frac)
      li <- li + 1
    }
    if (li > length(lv)) break
  }
  list(levels = lv[seq_along(out_a)], areas = out_a, n_hulls = out_n, coverage = cover, hull_areas = areas)
}
