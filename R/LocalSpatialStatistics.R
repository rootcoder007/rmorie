.lss_graph_lags <- function(A, order) {
  n <- nrow(A)
  lags <- rep(list(matrix(0, n, n)), order)
  for (i in seq_len(n)) {
    dist <- rep(NA_integer_, n)
    dist[i] <- 0L
    frontier <- i
    for (k in seq_len(order)) {
      nxt <- integer(0)
      for (u in frontier) {
        for (v in which(A[u, ] != 0 & seq_len(n) != u)) {
          if (is.na(dist[v])) {
            dist[v] <- k
            nxt <- c(nxt, v)
            lags[[k]][i, v] <- 1
          }
        }
      }
      frontier <- nxt
    }
  }
  lags
}

.lss_style <- function(B, style) {
  rs <- rowSums(B)
  switch(style,
    B = B,
    W = B / ifelse(rs > 0, rs, 1),
    C = B * sum(rs > 0) / sum(B),
    U = B / sum(B),
    stop("style must be one of B, W, C, U")
  )
}

.lss_constants <- function(W) {
  rs <- rowSums(W)
  cs <- colSums(W)
  c(n = sum(apply(W != 0, 1, any)), s0 = sum(W), s1 = 0.5 * sum((W + t(W))^2), s2 = sum((rs + cs)^2))
}

.lss_moran <- function(x, W, randomisation) {
  k <- .lss_constants(W)
  n <- k[["n"]]
  s0 <- k[["s0"]]
  s1 <- k[["s1"]]
  s2 <- k[["s2"]]
  z <- x - mean(x)
  zz <- sum(z^2)
  K <- length(x) * sum(z^4) / zz^2
  stat <- (n / s0) * sum(z * as.vector(W %*% z)) / zz
  e <- -1 / (n - 1)
  if (randomisation) {
    v <- n * (s1 * (n^2 - 3 * n + 3) - n * s2 + 3 * s0^2)
    v <- v - K * (s1 * (n^2 - n) - 2 * n * s2 + 6 * s0^2)
    v <- v / ((n - 1) * (n - 2) * (n - 3) * s0^2) - e^2
  } else {
    v <- (n^2 * s1 - n * s2 + 3 * s0^2) / (s0^2 * (n^2 - 1)) - e^2
  }
  c(stat, e, v)
}

.lss_geary <- function(x, W, randomisation) {
  k <- .lss_constants(W)
  n <- k[["n"]]
  s0 <- k[["s0"]]
  s1 <- k[["s1"]]
  s2 <- k[["s2"]]
  N <- length(x)
  z <- (x - mean(x)) / stats::sd(x)
  zz <- sum(z^2)
  K <- n * sum(z^4) / zz^2
  n1 <- n - 1
  stat <- (n1 / (2 * s0)) * sum(W * outer(z, z, "-")^2) / zz
  if (randomisation) {
    v <- n1 * s1 * (n^2 - 3 * N + 3 - K * n1)
    v <- v - 0.25 * (n1 * s2 * (n^2 + 3 * N - 6 - K * (n^2 - N + 2)))
    v <- v + s0^2 * (n^2 - 3 - K * n1^2)
    v <- v / (N * (n - 2) * (n - 3) * s0^2)
  } else {
    v <- ((2 * s1 + s2) * n1 - 4 * s0^2) / (2 * (N + 1) * s0^2)
  }
  c(stat, 1, v)
}

#' Spatial correlogram over neighbour-graph lag orders
#'
#' For each lag \eqn{k = 1, \dots, order} the units at shortest-path
#' distance exactly \eqn{k} in the neighbour graph (\code{spdep::nblag}) are
#' coded with \code{style} and a global statistic is computed, as
#' \code{spdep::sp.correlogram}: Moran's I with expectation \eqn{-1/(n-1)}
#' and its randomisation or normality variance (\code{spdep::moran.test}),
#' Geary's C with expectation 1 (\code{spdep::geary.test}), or the Pearson
#' correlation of x with its lag-k spatial lag. \eqn{n} in the moments
#' counts units with a lag-k neighbour. The deviate is
#' (estimate - expectation) / sqrt(variance) with a two-sided normal
#' p-value.
#'
#' @param adjacency Neighbour indicator matrix (nonzero = neighbour).
#' @param x Numeric vector.
#' @param order Largest lag order.
#' @param method \code{"I"}, \code{"C"} or \code{"corr"}.
#' @param style Weight coding \code{"W"}, \code{"B"}, \code{"C"} or
#'   \code{"U"}.
#' @param randomisation Randomisation (else normality) variance.
#' @return List with \code{estimate}, \code{expectation}, \code{variance},
#'   \code{z}, \code{p_value} (by lag; only \code{estimate} for
#'   \code{"corr"}), \code{n_with_neighbours}, \code{method}, \code{style}.
#' @references Cliff, A. D. and Ord, J. K. (1981). Spatial Processes:
#'   Models and Applications. Pion, London.
#' @examples
#' A <- 1 * (abs(outer(1:8, 1:8, "-")) == 1)
#' SpatialCorrelogram(A, c(1, 2, 3, 5, 4, 6, 8, 7), order = 2)$estimate
#' @export
SpatialCorrelogram <- function(adjacency, x, order = 1L, method = "I", style = "W", randomisation = TRUE) {
  x <- as.numeric(x)
  A <- as.matrix(adjacency)
  N <- length(x)
  if (!identical(dim(A), c(N, N))) stop("adjacency must be n x n with n = length(x)")
  if (order < 1) stop("order must be at least 1")
  if (!method %in% c("I", "C", "corr")) stop("method must be I, C or corr")
  lags <- .lss_graph_lags(A, as.integer(order))
  est <- ex <- va <- zs <- ps <- numeric(0)
  counts <- integer(0)
  for (k in seq_along(lags)) {
    cnt <- sum(rowSums(lags[[k]]) > 0)
    if (cnt < 3) stop(sprintf("too few units with lag-%d neighbours; reduce order", k))
    counts <- c(counts, cnt)
    W <- .lss_style(lags[[k]], style)
    if (method == "corr") {
      est <- c(est, stats::cor(x, as.vector(W %*% x)))
      next
    }
    r <- if (method == "I") .lss_moran(x, W, randomisation) else .lss_geary(x, W, randomisation)
    z <- (r[1] - r[2]) / sqrt(r[3])
    est <- c(est, r[1])
    ex <- c(ex, r[2])
    va <- c(va, r[3])
    zs <- c(zs, z)
    ps <- c(ps, 2 * stats::pnorm(abs(z), lower.tail = FALSE))
  }
  list(estimate = est, expectation = ex, variance = va, z = zs, p_value = ps,
       n_with_neighbours = counts, method = method, style = style)
}

.lss_pairs <- function(v, f) {
  out <- NULL
  for (i in 2:length(v)) for (j in 1:(i - 1)) out <- c(out, f(v[i], v[j]))
  out
}

#' Join counts for a map of k colours
#'
#' \eqn{J_{rr}} counts joins between two units of colour r and
#' \eqn{J_{rs}} (r after s) joins between colours r and s, each as half the
#' weight sum over ordered pairs; Jtot sums the different-colour joins.
#' Expectations and variances are the nonfree sampling moments of Cliff
#' and Ord (1981, pp. 19-20), exactly as \code{spdep::joincount.multi}
#' (N counts the units with neighbours); deviates are
#' (J - E) / sqrt(Var). Colours are the sorted distinct labels.
#'
#' @param labels Colour of each unit.
#' @param W Spatial weights matrix; the diagonal is ignored.
#' @return List with \code{rows}, \code{joincount}, \code{expected},
#'   \code{variance}, \code{z}, \code{levels}.
#' @references Cliff, A. D. and Ord, J. K. (1981). Spatial Processes:
#'   Models and Applications. Pion, London.
#' @examples
#' W <- 1 * (abs(outer(1:9, 1:9, "-")) == 1)
#' JoinCountMulti(strsplit("aabbbccaa", "")[[1]], W)$joincount
#' @export
JoinCountMulti <- function(labels, W) {
  W <- as.matrix(W)
  n <- length(labels)
  if (!identical(dim(W), c(n, n))) stop("W must be n x n with n = length(labels)")
  diag(W) <- 0
  levels <- sort(unique(labels), method = "radix")
  k <- length(levels)
  if (k < 2) stop("need at least two colours")
  ci <- match(labels, levels)
  res <- matrix(0, k, k)
  for (r in seq_len(k)) for (s in seq_len(k)) res[r, s] <- sum(W[ci == r, ci == s]) / 2
  ntab <- tabulate(ci, k)
  wc <- .lss_constants(W)
  N <- wc[["n"]]
  s0 <- wc[["s0"]]
  s1 <- wc[["s1"]]
  s2 <- wc[["s2"]]
  n1 <- N - 1
  n2 <- N - 2
  n3 <- N - 3
  sq <- s0^2
  a <- ntab
  ejc <- s0 * a * (a - 1) / (2 * N * n1)
  vjc <- s1 * a * (a - 1) / (N * n1) + (s2 - 2 * s1) * a * (a - 1) * (a - 2) / (N * n1 * n2) +
    (sq + s1 - s2) * a * (a - 1) * (a - 2) * (a - 3) / (N * n1 * n2 * n3)
  vjc <- 0.25 * vjc - ejc^2
  idx <- seq_len(k)
  ldiag <- .lss_pairs(idx, function(i, j) res[i, j] + res[j, i])
  nm <- .lss_pairs(as.character(levels), function(i, j) paste(i, j, sep = ":"))
  pr <- .lss_pairs(a, `*`)
  plus <- .lss_pairs(a, `+`)
  prod2 <- .lss_pairs(a * (a - 1), `*`)
  ex <- s0 * pr / (N * n1)
  d3 <- N * n1 * n2 * n3
  vr <- 2 * s1 * pr / (N * n1) + (s2 - 2 * s1) * pr * (plus - 2) / (N * n1 * n2) +
    4 * (sq + s1 - s2) * prod2 / d3
  vr <- 0.25 * vr - ex^2
  jvar <- (s2 / (N * n1) - 4 * (sq + s1 - s2) * n1 / d3) * sum(pr)
  jvar <- jvar + 4 * ((s1 - s2) / d3 + 2 * sq * (2 * n - 3) / ((N * n1) * d3)) * sum(.lss_pairs(a^2, `*`))
  if (k > 2) {
    t3 <- sum(apply(utils::combn(k, 3), 2, function(cc) prod(a[cc])))
    jvar <- jvar + ((2 * s1 - 5 * s2) / (N * n1 * n2) + 12 * (sq + s1 - s2) / d3 + 8 * sq / ((N * n1 * n2) * n1)) * t3
  }
  if (k > 3) {
    t4 <- sum(apply(utils::combn(k, 4), 2, function(cc) prod(a[cc])))
    jvar <- jvar - 8 * ((s1 - s2) / d3 + 2 * sq * (2 * N - 3) / ((N * n1) * d3)) * t4
  }
  jvar <- 0.25 * jvar
  jc <- c(diag(res), ldiag, sum(ldiag))
  ee <- c(ejc, ex, sum(ex))
  vv <- c(vjc, vr, jvar)
  list(rows = c(paste(levels, levels, sep = ":"), nm, "Jtot"), joincount = jc, expected = ee,
       variance = vv, z = ifelse(vv > 0, (jc - ee) / sqrt(pmax(vv, 0)), NaN), levels = levels)
}

#' Local bivariate Moran statistic with conditional permutation
#'
#' \eqn{I^B_i = x_i \sum_j w_{ij} y_j} with x and y standardised when
#' \code{scale}, as \code{spdep::localmoran_bv}. For unit i the neighbour
#' values of y are redrawn \code{nsim} times with replacement from the other
#' n - 1 values; draws come from Philox stream i of \code{seed} (index
#' floor(u (n - 1))), identical to the Python arm (spdep uses R's
#' generator). Reported per unit: permutation mean and variance, deviate,
#' two-sided normal p-value and folded pseudo p-value
#' (min(#sim >= I, nsim - #sim >= I) + 1) / (nsim + 1). Quadrants compare x
#' with its mean and the unscaled lag of y with the lag's mean (ties are
#' Low).
#'
#' @param x Values at the focal unit.
#' @param y Values whose spatial lag is taken.
#' @param W Spatial weights; every unit needs a neighbour.
#' @param nsim Permutations per unit.
#' @param seed Philox seed.
#' @param scale Standardise x and y first.
#' @return List with \code{local_values}, \code{statistic} (mean),
#'   \code{expected}, \code{variance}, \code{z}, \code{p_normal},
#'   \code{p_folded}, \code{quadrant}, \code{nsim}.
#' @references Anselin, L., Syabri, I. and Smirnov, O. (2002). Visualizing
#'   multivariate spatial correlation with dynamically linked windows. New
#'   Tools for Spatial Data Analysis: Proceedings of the Specialist
#'   Meeting. CSISS, Santa Barbara.
#' @examples
#' W <- matrix(0, 4, 4); W[cbind(1:3, 2:4)] <- 1; W <- W + t(W)
#' LocalMoranBivariate(c(1, 2, 4, 8), c(2, 1, 5, 7), W, nsim = 9)$local_values
#' @export
LocalMoranBivariate <- function(x, y, W, nsim = 199L, seed = 1L, scale = TRUE) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  W <- as.matrix(W)
  n <- length(x)
  if (length(y) != n || !identical(dim(W), c(n, n))) stop("x, y and W must share n")
  diag(W) <- 0
  nb <- lapply(seq_len(n), function(i) which(W[i, ] != 0))
  if (any(lengths(nb) == 0L)) stop("every unit needs at least one neighbour")
  if (nsim < 1) stop("nsim must be at least 1")
  ly0 <- as.vector(W %*% y)
  quad <- paste(ifelse(x > mean(x), "High", "Low"), ifelse(ly0 > mean(ly0), "High", "Low"), sep = "-")
  if (scale) {
    x <- (x - mean(x)) / stats::sd(x)
    y <- (y - mean(y)) / stats::sd(y)
  }
  obs <- x * as.vector(W %*% y)
  ns <- as.integer(nsim)
  ex <- va <- zz <- pn <- pf <- numeric(n)
  for (i in seq_len(n)) {
    yi <- y[-i]
    wi <- W[i, nb[[i]]]
    cc <- length(wi)
    u <- .morie_random_uniform(ns * cc, seed = seed, stream = i - 1L)
    j <- pmin(floor(u * (n - 1)), n - 2) + 1
    draws <- matrix(yi[j], nrow = ns, ncol = cc, byrow = TRUE)
    sims <- x[i] * as.vector(draws %*% wi)
    m <- mean(sims)
    v <- if (ns > 1) stats::var(sims) else NaN
    z <- if (is.finite(v) && v > 0) (obs[i] - m) / sqrt(v) else NaN
    ge <- sum(sims >= obs[i])
    ex[i] <- m
    va[i] <- v
    zz[i] <- z
    pn[i] <- if (is.nan(z)) NaN else 2 * stats::pnorm(abs(z), lower.tail = FALSE)
    pf[i] <- (min(ge, ns - ge) + 1) / (ns + 1)
  }
  list(local_values = obs, statistic = mean(obs), expected = ex, variance = va, z = zz,
       p_normal = pn, p_folded = pf, quadrant = quad, nsim = ns)
}

#' Moran scatterplot coordinates and influence measures
#'
#' Pairs x with its spatial lag Wx (or the lag Wy of a second variable);
#' with row-standardised W the least-squares slope is Moran's I (Anselin
#' 1996). As \code{spdep::moran.plot}, the line Wx ~ x is fitted and each
#' point gets the \code{stats::influence.measures} diagnostics (DFBETAS,
#' DFFIT, COVRATIO, Cook's distance, hat value) and the \code{is_inf}
#' flag of \code{stats::influence.measures}.
#'
#' @param x Numeric vector.
#' @param W Spatial weights matrix.
#' @param y Optional second variable whose lag is the vertical axis.
#' @return List with \code{x}, \code{wx}, \code{slope}, \code{intercept},
#'   \code{dfb_1}, \code{dfb_x}, \code{dffit}, \code{cov_r},
#'   \code{cook_d}, \code{hat}, \code{is_inf}.
#' @references Anselin, L. (1996). The Moran scatterplot as an ESDA tool to
#'   assess local instability in spatial association. Spatial Analytical
#'   Perspectives on GIS, 111-125. Taylor and Francis, London.
#'
#'   Belsley, D. A., Kuh, E. and Welsch, R. E. (1980). Regression
#'   Diagnostics. Wiley, New York.
#' @examples
#' W <- matrix(c(0, 1, 0, 0, .5, 0, .5, 0, 0, .5, 0, .5, 0, 0, 1, 0), 4, byrow = TRUE)
#' MoranScatter(c(1, 2, 4, 3), W)$slope
#' @export
MoranScatter <- function(x, W, y = NULL) {
  x <- as.numeric(x)
  W <- as.matrix(W)
  n <- length(x)
  if (!identical(dim(W), c(n, n))) stop("W must be n x n with n = length(x)")
  if (n < 4) stop("need at least four units")
  src <- if (is.null(y)) x else as.numeric(y)
  if (length(src) != n) stop("y must have length n")
  diag(W) <- 0
  wx <- as.vector(W %*% src)
  sxx <- sum((x - mean(x))^2)
  if (sxx == 0) stop("x is constant")
  b <- sum((x - mean(x)) * (wx - mean(wx))) / sxx
  a <- mean(wx) - b * mean(x)
  e <- wx - a - b * x
  p <- 2
  rss <- sum(e^2)
  s <- sqrt(rss / (n - p))
  XtXi <- solve(crossprod(cbind(1, x)))
  h <- 1 / n + (x - mean(x))^2 / sxx
  si <- sqrt((rss - e^2 / (1 - h)) / (n - p - 1))
  g <- t(XtXi %*% rbind(1, x)) * (e / (1 - h))
  d1 <- g[, 1] / (si * sqrt(XtXi[1, 1]))
  d2 <- g[, 2] / (si * sqrt(XtXi[2, 2]))
  dff <- e * sqrt(h) / (si * (1 - h))
  cvr <- (si / s)^(2 * p) / (1 - h)
  cook <- (e / (s * (1 - h)))^2 * h / p
  inf <- abs(d1) > 1 | abs(d2) > 1 | abs(dff) > 3 * sqrt(p / (n - p)) |
    abs(1 - cvr) > 3 * p / (n - p) | stats::pf(cook, p, n - p) > 0.5 | h > 3 * p / n
  list(x = x, wx = wx, slope = b, intercept = a, dfb_1 = d1, dfb_x = d2, dffit = dff,
       cov_r = cvr, cook_d = cook, hat = h, is_inf = inf)
}
