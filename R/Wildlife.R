.wl_grad_hess <- function(f, x, h = 1e-4) {
  k <- length(x)
  g <- numeric(k)
  H <- matrix(0, k, k)
  f0 <- f(x)
  for (a in seq_len(k)) {
    ha <- h * max(1, abs(x[a]))
    xp <- x
    xm <- x
    xp[a] <- xp[a] + ha
    xm[a] <- xm[a] - ha
    fp <- f(xp)
    fm <- f(xm)
    g[a] <- (fp - fm) / (2 * ha)
    H[a, a] <- (fp - 2 * f0 + fm) / ha^2
    for (b in seq_len(a - 1)) {
      hb <- h * max(1, abs(x[b]))
      v <- vapply(list(c(1, 1), c(1, -1), c(-1, 1), c(-1, -1)), function(s) {
        t <- x
        t[a] <- t[a] + s[1] * ha
        t[b] <- t[b] + s[2] * hb
        f(t)
      }, 0)
      H[a, b] <- H[b, a] <- (v[1] - v[2] - v[3] + v[4]) / (4 * ha * hb)
    }
  }
  list(g = g, H = H)
}

.wl_mle <- function(nll, x0) {
  x <- stats::optim(x0, nll, method = "BFGS", control = list(reltol = 1e-14, maxit = 1000))$par
  f <- nll(x)
  for (it in 1:50) {
    d <- .wl_grad_hess(nll, x)
    step <- tryCatch(solve(d$H, d$g), error = function(e) NULL)
    if (is.null(step)) break
    xn <- x - step
    fn <- nll(xn)
    if (!isTRUE(fn <= f)) break
    done <- max(abs(step)) < 1e-10
    x <- xn
    f <- fn
    if (done) break
  }
  H <- .wl_grad_hess(nll, x)$H
  V <- tryCatch(solve(H), error = function(e) NULL)
  se <- if (is.null(V)) rep(NaN, length(x)) else suppressWarnings(sqrt(diag(V)))
  list(par = x, value = f, se = se, vcov = V)
}

.wl_log1pexp <- function(z) ifelse(z > 0, z + log1p(exp(-z)), log1p(exp(z)))

#' Wildlife ecology: home range, distance sampling, occupancy, abundance and connectivity
#'
#' \code{McpHomeRange}: minimum convex polygon of the relocations within the
#' \code{percent} quantile of distance to the centroid (\code{adehabitatHR::mcp}).
#' \code{DistanceSampling}: half-normal or hazard-rate detection function by
#' maximum likelihood (\code{mrds} parameterisation), effective strip width /
#' detection radius, detection probability, density and abundance (Buckland et
#' al. 2001). \code{OccupancyModel}: single-season occupancy (MacKenzie et al.
#' 2002, as \code{unmarked::occu}). \code{NmixtureModel}: Poisson binomial
#' N-mixture (Royle 2004, as \code{unmarked::pcount}, K = max(y) + 100).
#' \code{CircuitResistance}: effective resistance between grid cells
#' (isolation by resistance, McRae 2006). \code{LeastCostPath}: Dijkstra least
#' cost path, cost surfaces and corridor. \code{ResistanceFromSuitability}:
#' Keeley et al. (2016) transform. \code{HabitatSuitabilityIndex}: geometric,
#' arithmetic or limiting-factor HSI. \code{PartialMantel}: partial Mantel
#' test (Smouse et al. 1986). \code{GeneFlowNm}: Wright's island-model
#' \eqn{Nm}. \code{HanskiConnectivity}: Hanski's (1994) connectivity.
#' Maximum likelihood uses BFGS followed by Newton steps; standard errors come
#' from the inverse Hessian. Identical to the Python arm
#' \code{morie.fn.wildlife}; grid cells and nodes are 1-based.
#'
#' @param xy Two-column relocations.
#' @param percent Percentage of relocations kept.
#' @param distances Perpendicular or radial distances.
#' @param width Truncation distance.
#' @param key \code{"hn"} (half-normal) or \code{"hr"} (hazard rate).
#' @param transect \code{"line"} or \code{"point"}.
#' @param effort Total line length or number of points.
#' @param area Area for abundance.
#' @param y Detection (0/1) or count matrix, sites by visits (\code{NA} allowed).
#' @param psi_covariates,p_covariates,lambda_covariates Site design matrices
#'   (default intercept only).
#' @param K Upper summation bound for abundance.
#' @param resistance,cost Resistance or cost grid.
#' @param nodes Two-column matrix of (row, col) cells.
#' @param directions 4 or 8 neighbours.
#' @param start,end Cells \code{c(row, col)}.
#' @param corridor_slack Corridor tolerance (default 10 percent of the least cost).
#' @param suitability Habitat suitability in the unit interval.
#' @param c Transform shape.
#' @param indices List of suitability index variables.
#' @param method HSI aggregation.
#' @param weights Variable weights.
#' @param A,B,C Distance matrices.
#' @param nsim Permutations.
#' @param seed Philox seed.
#' @param fst Fixation index.
#' @param coords Patch coordinates.
#' @param occupied Occupancy (0/1 or probability).
#' @param areas Patch areas.
#' @param alpha Inverse mean dispersal distance.
#' @param b Area scaling exponent.
#' @return List (or numeric vector).
#' @references Buckland, S. T. et al. (2001). Introduction to Distance
#'   Sampling. Oxford University Press.
#'
#'   MacKenzie, D. I. et al. (2002). Estimating site occupancy rates when
#'   detection probabilities are less than one. Ecology 83, 2248-2255.
#'
#'   Royle, J. A. (2004). N-mixture models for estimating population size from
#'   spatially replicated counts. Biometrics 60, 108-115.
#'
#'   McRae, B. H. (2006). Isolation by resistance. Evolution 60, 1551-1561.
#'
#'   Keeley, A. T. H., Beier, P. and Gagnon, J. W. (2016). Estimating landscape
#'   resistance from habitat suitability. Landscape Ecology 31, 2151-2162.
#'
#'   Smouse, P. E., Long, J. C. and Sokal, R. R. (1986). Multiple regression and
#'   correlation extensions of the Mantel test. Systematic Zoology 35, 627-632.
#'
#'   Hanski, I. (1994). A practical model of metapopulation dynamics. Journal of
#'   Animal Ecology 63, 151-162.
#' @examples
#' McpHomeRange(rbind(c(0, 0), c(2, 0), c(2, 2), c(0, 2), c(1, 1), c(10, 10)), 80)$area
#' CircuitResistance(matrix(1, 1, 3), rbind(c(1, 1), c(1, 3)))$resistance
#' GeneFlowNm(0.2)
#' @export
McpHomeRange <- function(xy, percent = 95) {
  P <- as.matrix(xy)
  if (nrow(P) < 5) stop("at least 5 relocations are required", call. = FALSE)
  d <- sqrt((P[, 1] - mean(P[, 1]))^2 + (P[, 2] - mean(P[, 2]))^2)
  keep <- P[d <= stats::quantile(d, min(percent, 100) / 100), , drop = FALSE]
  h <- grDevices::chull(keep)
  H <- keep[h, , drop = FALSE]
  n <- nrow(H)
  j <- c(2:n, 1)
  area <- abs(sum(H[, 1] * H[j, 2] - H[j, 1] * H[, 2])) / 2
  list(area = area, area_ha = area / 10000, hull = H, n_used = nrow(keep))
}

#' @rdname McpHomeRange
#' @export
DistanceSampling <- function(distances, width, key = "hn", transect = "line", effort = NULL, area = NULL) {
  key <- match.arg(key, c("hn", "hr"))
  transect <- match.arg(transect, c("line", "point"))
  x <- as.numeric(distances)
  x <- x[x <= width]
  n <- length(x)
  g <- function(d, th) {
    s <- exp(th[1])
    if (key == "hn") exp(-d^2 / (2 * s^2)) else ifelse(d > 0, 1 - exp(-(d / s)^(-exp(th[2]))), 1)
  }
  gl <- .wl_gl64()
  integ <- function(f) {
    h <- width / 16
    sum(vapply(0:15, function(p) sum(gl$w * f(p * h + h * (gl$x + 1) / 2)) * h / 2, 0))
  }
  norm <- function(th) {
    if (key == "hn") {
      s <- exp(th[1])
      if (transect == "line") return(s * sqrt(pi / 2) * (2 * stats::pnorm(width / s) - 1))
      return(2 * pi * s^2 * (1 - exp(-width^2 / (2 * s^2))))
    }
    if (transect == "line") integ(function(d) g(d, th)) else 2 * pi * integ(function(d) d * g(d, th))
  }
  nll <- function(th) {
    cc <- norm(th)
    if (!isTRUE(cc > 0)) return(Inf)
    gd <- g(x, th)
    if (any(gd <= 0)) return(Inf)
    -(-n * log(cc) + sum(log(gd)) + if (transect == "point") sum(log(2 * pi * x[x > 0])) else 0)
  }
  s0 <- if (n) sqrt(mean(x^2)) else width / 2
  fit <- .wl_mle(nll, c(log(s0), if (key == "hr") 0.5))
  cc <- norm(fit$par)
  P <- if (transect == "line") cc / width else cc / (pi * width^2)
  out <- list(sigma = exp(fit$par[1]), shape = if (key == "hr") exp(fit$par[2]) else NULL, coefficients = fit$par,
              se = fit$se, loglik = -fit$value, aic = 2 * fit$value + 2 * length(fit$par), p = P, n = n)
  if (transect == "line") out$esw <- cc else out$edr <- sqrt(cc / pi)
  if (!is.null(effort)) {
    out$density <- if (transect == "line") n / (2 * effort * cc) else n / (effort * cc)
    if (!is.null(area)) out$abundance <- out$density * area
  }
  out
}

.wl_gl64 <- function() {
  n <- 64
  xs <- ws <- numeric(n)
  for (i in seq_len(n)) {
    x <- cos(pi * (i - 0.25) / (n + 0.5))
    for (it in 1:100) {
      p0 <- 1
      p1 <- x
      for (k in 2:n) {
        p2 <- ((2 * k - 1) * x * p1 - (k - 1) * p0) / k
        p0 <- p1
        p1 <- p2
      }
      dp <- n * (x * p1 - p0) / (x^2 - 1)
      dx <- p1 / dp
      x <- x - dx
      if (abs(dx) < 1e-15) break
    }
    xs[i] <- x
    ws[i] <- 2 / ((1 - x^2) * dp^2)
  }
  list(x = xs, w = ws)
}

.wl_design <- function(X, n) if (is.null(X)) matrix(1, n, 1) else as.matrix(X)

#' @rdname McpHomeRange
#' @export
OccupancyModel <- function(y, psi_covariates = NULL, p_covariates = NULL) {
  Y <- as.matrix(y)
  n <- nrow(Y)
  Xs <- .wl_design(psi_covariates, n)
  Xd <- .wl_design(p_covariates, n)
  q <- ncol(Xs)
  nll <- function(th) {
    ps <- stats::plogis(drop(Xs %*% th[seq_len(q)]))
    eta <- drop(Xd %*% th[-seq_len(q)])
    tot <- 0
    for (i in seq_len(n)) {
      o <- Y[i, !is.na(Y[i, ])]
      lp <- sum(ifelse(o == 1, -.wl_log1pexp(-eta[i]), -.wl_log1pexp(eta[i])))
      like <- ps[i] * exp(lp) + (if (sum(o) == 0) 1 - ps[i] else 0)
      if (like <= 0) return(Inf)
      tot <- tot + log(like)
    }
    -tot
  }
  fit <- .wl_mle(nll, numeric(q + ncol(Xd)))
  b <- fit$par[seq_len(q)]
  a <- fit$par[-seq_len(q)]
  psi <- stats::plogis(drop(Xs %*% b))
  p <- stats::plogis(drop(Xd %*% a))
  m <- rowSums(!is.na(Y))
  det <- rowSums(Y, na.rm = TRUE) > 0
  cond <- ifelse(det, 1, psi * (1 - p)^m / (psi * (1 - p)^m + 1 - psi))
  list(psi_coefficients = b, p_coefficients = a, se = fit$se, loglik = -fit$value,
       aic = 2 * fit$value + 2 * length(fit$par), psi = psi, p = p, conditional_occupancy = cond,
       naive_occupancy = mean(det), occupancy = mean(psi))
}

#' @rdname McpHomeRange
#' @export
NmixtureModel <- function(y, lambda_covariates = NULL, p_covariates = NULL, K = NULL) {
  Y <- as.matrix(y)
  n <- nrow(Y)
  Xl <- .wl_design(lambda_covariates, n)
  Xd <- .wl_design(p_covariates, n)
  q <- ncol(Xl)
  if (is.null(K)) K <- max(Y, na.rm = TRUE) + 100
  nll <- function(th) {
    ll <- drop(Xl %*% th[seq_len(q)])
    eta <- drop(Xd %*% th[-seq_len(q)])
    tot <- 0
    for (i in seq_len(n)) {
      o <- Y[i, !is.na(Y[i, ])]
      N <- (if (length(o)) max(o) else 0):K
      t <- N * ll[i] - exp(ll[i]) - lgamma(N + 1)
      lp <- -.wl_log1pexp(-eta[i])
      lq <- -.wl_log1pexp(eta[i])
      for (v in o) t <- t + lgamma(N + 1) - lgamma(v + 1) - lgamma(N - v + 1) + v * lp + (N - v) * lq
      m <- max(t)
      tot <- tot + m + log(sum(exp(t - m)))
    }
    -tot
  }
  fit <- .wl_mle(nll, numeric(q + ncol(Xd)))
  b <- fit$par[seq_len(q)]
  a <- fit$par[-seq_len(q)]
  lam <- exp(drop(Xl %*% b))
  list(lambda_coefficients = b, p_coefficients = a, se = fit$se, loglik = -fit$value,
       aic = 2 * fit$value + 2 * length(fit$par), lambda = lam, p = stats::plogis(drop(Xd %*% a)),
       total_abundance = sum(lam), K = K)
}

#' @rdname McpHomeRange
#' @export
CircuitResistance <- function(resistance, nodes, directions = 4) {
  G <- as.matrix(resistance)
  ok <- is.finite(G) & G > 0
  rm_order <- which(t(ok))  # row-major numbering, as the Python arm
  id <- matrix(0L, nrow(G), ncol(G))
  cells <- arrayInd(rm_order, dim(t(G)))[, 2:1, drop = FALSE]
  for (u in seq_len(nrow(cells))) id[cells[u, 1], cells[u, 2]] <- u
  m <- nrow(cells)
  nb <- if (directions == 8) list(c(0, 1), c(1, 0), c(1, 1), c(1, -1)) else list(c(0, 1), c(1, 0))
  L <- matrix(0, m, m)
  for (u in seq_len(m)) {
    i <- cells[u, 1]
    j <- cells[u, 2]
    for (d in nb) {
      x <- i + d[1]
      y <- j + d[2]
      if (x >= 1 && x <= nrow(G) && y >= 1 && y <= ncol(G) && id[x, y] > 0) {
        v <- id[x, y]
        w <- 1 / ((if (d[1] != 0 && d[2] != 0) sqrt(2) else 1) * (G[i, j] + G[x, y]) / 2)
        L[u, u] <- L[u, u] + w
        L[v, v] <- L[v, v] + w
        L[u, v] <- L[u, v] - w
        L[v, u] <- L[v, u] - w
      }
    }
  }
  nodes <- as.matrix(nodes)
  nd <- id[nodes]
  k <- length(nd)
  R <- matrix(0, k, k)
  for (s in seq_len(k - 1)) for (t in (s + 1):k) {
    keep <- setdiff(seq_len(m), nd[t])
    e <- as.numeric(keep == nd[s])
    xv <- solve(L[keep, keep, drop = FALSE], e)
    R[s, t] <- R[t, s] <- xv[keep == nd[s]]
  }
  list(resistance = R, nodes = nodes)
}

.wl_dijkstra <- function(G, src, directions) {
  nr <- nrow(G)
  nc <- ncol(G)
  nb <- list()
  for (a in -1:1) for (b in -1:1) if ((a != 0 || b != 0) && (directions == 8 || a == 0 || b == 0)) nb[[length(nb) + 1]] <- c(a, b)
  dist <- matrix(Inf, nr, nc)
  prev <- array(NA_integer_, c(nr, nc, 2))
  done <- matrix(FALSE, nr, nc)
  dist[src[1], src[2]] <- 0
  repeat {
    cand <- which(!done & is.finite(dist))
    if (!length(cand)) break
    k <- cand[which.min(dist[cand])]
    i <- (k - 1) %% nr + 1
    j <- (k - 1) %/% nr + 1
    done[i, j] <- TRUE
    for (d in nb) {
      x <- i + d[1]
      y <- j + d[2]
      if (x >= 1 && x <= nr && y >= 1 && y <= nc && is.finite(G[x, y]) && !done[x, y]) {
        nd <- dist[i, j] + (if (d[1] != 0 && d[2] != 0) sqrt(2) else 1) * (G[i, j] + G[x, y]) / 2
        if (nd < dist[x, y] - 1e-15) {
          dist[x, y] <- nd
          prev[x, y, ] <- c(i, j)
        }
      }
    }
  }
  list(dist = dist, prev = prev)
}

#' @rdname McpHomeRange
#' @export
LeastCostPath <- function(cost, start, end, directions = 8, corridor_slack = NULL) {
  G <- as.matrix(cost)
  a <- .wl_dijkstra(G, start, directions)
  b <- .wl_dijkstra(G, end, directions)
  path <- list()
  cur <- end
  while (!is.na(cur[1])) {
    path[[length(path) + 1]] <- cur
    cur <- a$prev[cur[1], cur[2], ]
  }
  path <- do.call(rbind, rev(path))
  best <- a$dist[end[1], end[2]]
  slack <- if (is.null(corridor_slack)) 0.1 * best else corridor_slack
  list(path = unname(path), cost = best, cost_from_start = a$dist, cost_from_end = b$dist,
       corridor = a$dist + b$dist <= best + slack + 1e-12)
}

#' @rdname McpHomeRange
#' @export
ResistanceFromSuitability <- function(suitability, c = 8) {
  h <- as.numeric(suitability)
  if (c == 0) 100 - 99 * h else 100 - 99 * (1 - exp(-c * h)) / (1 - exp(-c))
}

#' @rdname McpHomeRange
#' @export
HabitatSuitabilityIndex <- function(indices, method = "geometric", weights = NULL) {
  method <- match.arg(method, c("geometric", "arithmetic", "minimum"))
  S <- do.call(cbind, lapply(indices, as.numeric))
  w <- if (is.null(weights)) rep(1, ncol(S)) else weights
  switch(method,
    geometric = apply(S, 1, function(v) if (min(v) <= 0) 0 else exp(sum(w * log(v)) / sum(w))),
    arithmetic = drop(S %*% w) / sum(w),
    minimum = apply(S, 1, min))
}

#' @rdname McpHomeRange
#' @export
PartialMantel <- function(A, B, C, nsim = 999L, seed = 1L) {
  low <- function(D) as.matrix(D)[lower.tri(as.matrix(D))]
  b <- low(B)
  cc <- low(C)
  rbc <- stats::cor(b, cc)
  st <- function(M) {
    a <- low(M)
    rab <- stats::cor(a, b)
    rac <- stats::cor(a, cc)
    (rab - rac * rbc) / sqrt((1 - rac^2) * (1 - rbc^2))
  }
  A <- as.matrix(A)
  n <- nrow(A)
  t0 <- st(A)
  sims <- vapply(seq_len(nsim), function(s) {
    u <- .morie_random_uniform(n, seed = seed, stream = s)
    p <- seq_len(n)
    for (i in seq_len(n - 1)) {
      k <- i + floor(u[i] * (n - i + 1))
      tmp <- p[i]
      p[i] <- p[k]
      p[k] <- tmp
    }
    st(A[p, p])
  }, 0)
  list(statistic = t0, pvalue = if (nsim > 0) (1 + sum(sims >= t0)) / (nsim + 1) else NaN)
}

#' @rdname McpHomeRange
#' @export
GeneFlowNm <- function(fst) (1 / fst - 1) / 4

#' @rdname McpHomeRange
#' @export
HanskiConnectivity <- function(coords, occupied, areas, alpha = 1, b = 0.5) {
  P <- as.matrix(coords)
  D <- as.matrix(stats::dist(P))
  vapply(seq_len(nrow(P)), function(i) sum((occupied * exp(-alpha * D[i, ]) * areas^b)[-i]), 0)
}
