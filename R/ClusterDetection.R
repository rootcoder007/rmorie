.cd_stat_poisson <- function(yin, ty, ein, eout, min_cases) {
  if (yin < min_cases || yin <= 0) return(0)
  yout <- ty - yin
  lrin <- log(yin) - log(ein)
  lrout <- if (yout > 0) log(yout) - log(eout) else -Inf
  if (lrin <= lrout) return(0)
  yin * lrin + if (yout > 0) yout * lrout else 0
}

.cd_pvals <- function(tobs, tmax) {
  if (!length(tmax)) return(rep(1, length(tobs)))
  vapply(tobs, function(x) (1 + sum(tmax >= x)) / (length(tmax) + 1), 0)
}

.cd_perm <- function(n, seed, stream) {
  u <- .morie_random_uniform(n, seed = seed, stream = stream)
  p <- seq_len(n)
  for (t in seq_len(n - 1)) {
    k <- t + floor(u[t] * (n - t + 1))
    tmp <- p[t]
    p[t] <- p[k]
    p[k] <- tmp
  }
  p
}

.cd_ell_nn <- function(P, pop, ubpop, shapes, nangles) {
  tp <- sum(pop)
  n <- nrow(P)
  nn <- list()
  shp <- ang <- numeric(0)
  for (si in seq_along(shapes)) {
    s <- shapes[si]
    na <- nangles[si]
    for (a in 90 + 180 * (seq_len(na) - 1) / na) {
      rad <- a * pi / 180
      sr <- sin(rad)
      cr <- cos(rad)
      for (i in seq_len(n)) {
        dx <- P[, 1] - P[i, 1]
        dy <- P[, 2] - P[i, 2]
        m1 <- (dx * cr + dy * sr) / s
        m2 <- dx * sr - dy * cr
        o <- order(sqrt(m1 * m1 + m2 * m2))
        nn[[length(nn) + 1]] <- o[cumsum(pop[o]) <= tp * ubpop]
        shp <- c(shp, s)
        ang <- c(ang, a)
      }
    }
  }
  list(nn = nn, shape = shp, angle = ang)
}

.cd_ell_stats <- function(nn, y, ex, ty, pen, a, min_cases) {
  lapply(seq_along(nn), function(k) {
    l <- nn[[k]]
    yin <- cumsum(y[l])
    ein <- cumsum(ex[l])
    t <- vapply(seq_along(l), function(m) .cd_stat_poisson(yin[m], ty, ein[m], ty - ein[m], min_cases), 0)
    if (a > 0 && pen[k] < 1) t * pen[k] else t
  })
}

.cd_noc_start <- function(nn, st) {
  start <- vapply(nn, function(l) if (length(l)) l[1] else NA_integer_, 0L)
  remaining <- which(lengths(nn) > 0)
  clusts <- list()
  tobs <- numeric(0)
  which_nn <- integer(0)
  cur <- 1
  while (length(remaining) && cur > 0) {
    mx <- vapply(remaining, function(i) if (length(st[[i]])) max(st[[i]]) else -Inf, 0)
    if (all(mx == -Inf)) break
    bi <- remaining[which.max(mx)]
    bk <- which.max(st[[bi]])
    clusts <- c(clusts, list(nn[[bi]][seq_len(bk)]))
    which_nn <- c(which_nn, bi)
    cur <- max(mx)
    tobs <- c(tobs, cur)
    used <- unlist(clusts)
    remaining <- which(lengths(nn) > 0 & !(start %in% used))
    for (i in remaining) {
      w <- which(nn[[i]] %in% used)
      if (length(w)) st[[i]] <- st[[i]][seq_len(w[1] - 1)]
    }
  }
  list(zones = clusts, tobs = tobs, which = which_nn)
}

#' Cluster detection beyond the circular scan
#'
#' \code{EllipticScan}: Kulldorff's elliptic scan (as
#' \code{smerc::elliptic.test}); zones are nearest-neighbour prefixes in the
#' elliptic distance for each shape and angle, the Poisson likelihood ratio is
#' multiplied by the penalty \eqn{(4s/(s+1)^2)^a}, clusters are chosen
#' greedily without overlap and p-values come from multinomial
#' redistributions of the cases (Philox). \code{FlexScan}: Tango and
#' Takahashi's flexibly shaped scan (as \code{smerc::flex.test}) over the
#' connected subsets of each region's \code{k} nearest neighbours;
#' \code{FlexZones} lists those zones (1-based). \code{NormalScan}: the
#' normal-model scan for continuous outcomes, \eqn{N\ln(s_0/s_z)} over
#' circular zones, with a permutation p-value. \code{CuzickEdwards}: the
#' \eqn{T_k} count of case-case k-nearest-neighbour pairs, its expectation
#' \eqn{k n_1(n_1-1)/(n-1)} and a permutation p-value. \code{LawsonWaller}:
#' the focused score statistic \eqn{U = \sum c_i(O_i - E_i)} with
#' multinomial (conditional) or Poisson variance. \code{FixedCircleScan}:
#' Openshaw's GAM (Poisson tail) or the Rushton-Lolonis Monte Carlo test on
#' circles of fixed radii centred on a grid of spacing \code{overlap * r}.
#' Regions are 1-based. Identical to the Python arm
#' \code{morie.fn.clusterdetect}.
#'
#' @param coords Two-column coordinates.
#' @param cases Case counts (or indicators for \code{CuzickEdwards}).
#' @param pop Populations.
#' @param ex Expected counts (default proportional to \code{pop}).
#' @param ubpop Maximum population share of a zone.
#' @param shape Ellipse shapes (ratio of axes).
#' @param nangle Number of angles per shape.
#' @param a Eccentricity penalty exponent.
#' @param nsim Monte Carlo replicates.
#' @param alpha Significance level.
#' @param min_cases Minimum cases in a zone.
#' @param seed Philox seed.
#' @param w Adjacency matrix.
#' @param k Number of nearest neighbours.
#' @param kind \code{"poisson"} or \code{"binomial"}.
#' @param values Continuous outcome, one per location.
#' @param direction \code{"high"}, \code{"low"} or \code{"both"}.
#' @param case Case indicator (1 case, 0 control).
#' @param expected Expected counts.
#' @param exposure Exposure covariate \eqn{c_i}.
#' @param conditional Condition on the total number of cases.
#' @param radii Circle radii.
#' @param overlap Grid spacing as a fraction of the radius.
#' @param method \code{"gam"} or \code{"rushton_lolonis"}.
#' @return List.
#' @references Kulldorff, M., Huang, L., Pickle, L. and Duczmal, L. (2006). An
#'   elliptic spatial scan statistic. Statistics in Medicine 25, 3929-3943.
#'
#'   Tango, T. and Takahashi, K. (2005). A flexibly shaped spatial scan
#'   statistic for detecting clusters. International Journal of Health
#'   Geographics 4, 11.
#'
#'   Kulldorff, M., Huang, L. and Konty, K. (2009). A scan statistic for
#'   continuous data based on the normal probability model. International
#'   Journal of Health Geographics 8, 58.
#'
#'   Cuzick, J. and Edwards, R. (1990). Spatial clustering for inhomogeneous
#'   populations. Journal of the Royal Statistical Society B 52, 73-104.
#'
#'   Waller, L. A., Turnbull, B. W., Clark, L. C. and Nasca, P. (1992). Chronic
#'   disease surveillance and testing of clustering of disease and exposure.
#'   Environmetrics 3, 281-300.
#'
#'   Openshaw, S., Charlton, M., Wymer, C. and Craft, A. (1987). A Mark 1
#'   Geographical Analysis Machine for the automated analysis of point data
#'   sets. International Journal of Geographical Information Systems 1,
#'   335-358.
#'
#'   Rushton, G. and Lolonis, P. (1996). Exploratory spatial analysis of birth
#'   defect rates in an urban population. Statistics in Medicine 15, 717-726.
#' @examples
#' LawsonWaller(c(4, 2, 1, 1), rep(2, 4), c(1, 0.5, 0.25, 0.25))$U
#' P <- cbind(0:4, 0)
#' W <- 1 * (abs(outer(1:5, 1:5, `-`)) == 1)
#' FlexScan(P, c(8, 7, 1, 1, 1), rep(10, 5), W, k = 3, nsim = 0)$clusters[[1]]$zone
#' @export
EllipticScan <- function(coords, cases, pop, ex = NULL, ubpop = 0.5, shape = c(1, 1.5, 2, 3, 4, 5),
                         nangle = c(1, 4, 6, 9, 12, 15), a = 0.5, nsim = 499L, alpha = 0.1, min_cases = 2,
                         seed = 1L) {
  P <- as.matrix(coords)
  y <- as.numeric(cases)
  pp <- as.numeric(pop)
  ty <- sum(y)
  e <- if (is.null(ex)) pp * ty / sum(pp) else as.numeric(ex)
  E <- .cd_ell_nn(P, pp, ubpop, shape, nangle)
  pen <- (4 * E$shape / (E$shape + 1)^2)^a
  nc <- .cd_noc_start(E$nn, .cd_ell_stats(E$nn, y, e, ty, pen, a, min_cases))
  tmax <- vapply(seq_len(nsim), function(s) {
    ys <- .scan_multinom(ty, e, seed, s)
    max(0, unlist(.cd_ell_stats(E$nn, ys, e, ty, pen, a, min_cases)))
  }, 0)
  pv <- .cd_pvals(nc$tobs, tmax)
  sig <- which(pv <= alpha)
  if (!length(sig)) sig <- which.min(pv)
  cl <- lapply(sig, function(i) list(zone = nc$zones[[i]], llr = nc$tobs[i], pvalue = pv[i],
                                     shape = E$shape[nc$which[i]], angle = E$angle[nc$which[i]],
                                     cases = sum(y[nc$zones[[i]]]), expected = sum(e[nc$zones[[i]]])))
  list(clusters = cl, all_zones = nc$zones, all_tobs = nc$tobs, all_pvalues = pv, all_shapes = E$shape[nc$which],
       all_angles = E$angle[nc$which])
}

#' @rdname EllipticScan
#' @export
FlexZones <- function(coords, w, k = 10) {
  P <- as.matrix(coords)
  n <- nrow(P)
  A <- as.matrix(w) != 0
  D <- as.matrix(stats::dist(P))
  out <- list()
  seen <- character(0)
  key <- function(z) paste(sort(z), collapse = ",")
  for (i in seq_len(n)) {
    knn <- order(D[i, ])[seq_len(k)]
    level <- list(knn[1])
    if (!(key(knn[1]) %in% seen)) {
      seen <- c(seen, key(knn[1]))
      out[[length(out) + 1]] <- knn[1]
    }
    for (step in seq_len(k - 1)) {
      nxt <- list()
      lev_seen <- character(0)
      for (z in level) for (v in knn) {
        if (!(v %in% z) && any(A[v, z])) {
          z2 <- sort(c(z, v))
          kz <- key(z2)
          if (!(kz %in% lev_seen)) {
            lev_seen <- c(lev_seen, kz)
            nxt[[length(nxt) + 1]] <- z2
          }
        }
      }
      if (!length(nxt)) break
      for (z in nxt) if (!(key(z) %in% seen)) {
        seen <- c(seen, key(z))
        out[[length(out) + 1]] <- z
      }
      level <- nxt
    }
  }
  out
}

#' @rdname EllipticScan
#' @export
FlexScan <- function(coords, cases, pop, w, k = 10, ex = NULL, kind = "poisson", nsim = 499L, alpha = 0.1,
                     seed = 1L) {
  kind <- match.arg(kind, c("poisson", "binomial"))
  y <- as.numeric(cases)
  pp <- as.numeric(pop)
  ty <- sum(y)
  tpop <- sum(pp)
  e <- if (is.null(ex)) pp * ty / tpop else as.numeric(ex)
  zones <- FlexZones(coords, w, k)
  ein <- vapply(zones, function(z) sum(e[z]), 0)
  pin <- vapply(zones, function(z) sum(pp[z]), 0)
  stats <- function(yy) vapply(seq_along(zones), function(i) {
    yin <- sum(yy[zones[[i]]])
    if (kind == "poisson") .cd_stat_poisson(yin, ty, ein[i], ty - ein[i], 1) else
      .scan_llr(yin, ty, 0, pin[i], tpop, "binomial", 1)
  }, 0)
  tobs <- stats(y)
  tmax <- if (nsim > 1) vapply(seq_len(nsim), function(s) max(stats(.scan_multinom(ty, e, seed, s))), 0) else numeric(0)
  pv <- .cd_pvals(tobs, tmax)
  sig <- which(pv <= alpha)
  if (!length(sig)) sig <- order(pv, -tobs)[1]
  sig <- sig[order(-tobs[sig])]
  keep <- integer(0)
  used <- integer(0)
  for (i in sig) if (!any(zones[[i]] %in% used)) {
    keep <- c(keep, i)
    used <- c(used, zones[[i]])
  }
  cl <- lapply(keep, function(i) list(zone = zones[[i]], llr = tobs[i], pvalue = pv[i], cases = sum(y[zones[[i]]]),
                                      expected = ein[i]))
  list(clusters = cl, zones = zones, tobs = tobs, pvalues = pv)
}

#' @rdname EllipticScan
#' @export
NormalScan <- function(coords, values, pop = NULL, ubpop = 0.5, direction = "high", nsim = 499L, seed = 1L) {
  direction <- match.arg(direction, c("high", "low", "both"))
  P <- as.matrix(coords)
  x <- as.numeric(values)
  N <- length(x)
  pp <- if (is.null(pop)) rep(1, N) else as.numeric(pop)
  nn <- .scan_nn(P, pp, ubpop)
  tot <- sum(x)
  tot2 <- sum(x^2)
  best <- function(xx) {
    s0 <- tot2 / N - (tot / N)^2
    bt <- 0
    bz <- NULL
    for (l in nn) {
      si <- cumsum(xx[l])
      si2 <- cumsum(xx[l]^2)
      for (m in seq_along(l)) {
        if (m == N) next
        mi <- si[m] / m
        mo <- (tot - si[m]) / (N - m)
        if ((direction == "high" && mi <= mo) || (direction == "low" && mi >= mo) || mi == mo) next
        sz <- (si2[m] - m * mi^2 + (tot2 - si2[m]) - (N - m) * mo^2) / N
        t <- if (sz > 0) N * 0.5 * (log(s0) - log(sz)) else Inf
        if (t > bt) {
          bt <- t
          bz <- l[seq_len(m)]
        }
      }
    }
    list(t = bt, z = bz)
  }
  b <- best(x)
  tmax <- vapply(seq_len(nsim), function(s) best(x[.cd_perm(N, seed, s)])$t, 0)
  list(zone = b$z, llr = b$t, pvalue = .cd_pvals(b$t, tmax), mean_in = if (length(b$z)) mean(x[b$z]) else NaN)
}

#' @rdname EllipticScan
#' @export
CuzickEdwards <- function(coords, case, k = 1, nsim = 999L, seed = 1L) {
  P <- as.matrix(coords)
  d <- as.integer(case)
  n <- length(d)
  D <- as.matrix(stats::dist(P))
  nb <- lapply(seq_len(n), function(i) {
    o <- order(D[i, ])
    o[o != i][seq_len(k)]
  })
  Tk <- function(lab) sum(vapply(seq_len(n), function(i) if (lab[i]) sum(lab[nb[[i]]]) else 0L, 0L))
  t <- Tk(d)
  n1 <- sum(d)
  sims <- vapply(seq_len(nsim), function(s) Tk(d[.cd_perm(n, seed, s)]), 0L)
  list(statistic = t, expected = k * n1 * (n1 - 1) / (n - 1),
       pvalue = if (nsim > 0) (1 + sum(sims >= t)) / (nsim + 1) else NaN)
}

#' @rdname EllipticScan
#' @export
LawsonWaller <- function(cases, expected, exposure, conditional = TRUE) {
  O <- as.numeric(cases)
  E <- as.numeric(expected)
  cc <- as.numeric(exposure)
  if (conditional) E <- E * sum(O) / sum(E)
  U <- sum(cc * (O - E))
  V <- sum(cc^2 * E)
  if (conditional) V <- V - sum(cc * E)^2 / sum(E)
  z <- U / sqrt(V)
  list(U = U, variance = V, z = z, pvalue = 1 - stats::pnorm(z))
}

#' @rdname EllipticScan
#' @export
FixedCircleScan <- function(coords, cases, pop, radii, overlap = 0.2, method = "gam", alpha = 0.002, nsim = 99L,
                            seed = 1L) {
  method <- match.arg(method, c("gam", "rushton_lolonis"))
  P <- as.matrix(coords)
  y <- as.numeric(cases)
  pp <- as.numeric(pop)
  Y <- sum(y)
  tp <- sum(pp)
  sims <- if (method == "rushton_lolonis") lapply(seq_len(nsim), function(s) .scan_multinom(Y, pp, seed, s)) else list()
  out <- list()
  ntest <- 0
  for (r in radii) {
    step <- overlap * r
    nx <- floor((max(P[, 1]) - min(P[, 1])) / step + 1e-9) + 1
    ny <- floor((max(P[, 2]) - min(P[, 2])) / step + 1e-9) + 1
    for (a in seq_len(nx) - 1) for (b in seq_len(ny) - 1) {
      cx <- min(P[, 1]) + a * step
      cy <- min(P[, 2]) + b * step
      mem <- which(sqrt((P[, 1] - cx)^2 + (P[, 2] - cy)^2) <= r)
      if (!length(mem)) next
      ntest <- ntest + 1
      O <- sum(y[mem])
      E <- sum(pp[mem]) * Y / tp
      pv <- if (method == "gam") {
        if (O <= 0) 1 else stats::ppois(O - 1, E, lower.tail = FALSE)
      } else {
        (1 + sum(vapply(sims, function(s) sum(s[mem]) >= O, TRUE))) / (nsim + 1)
      }
      if (pv <= alpha) out[[length(out) + 1]] <- list(center = c(cx, cy), radius = r, members = mem, observed = O,
                                                      expected = E, pvalue = pv)
    }
  }
  list(circles = out, n_circles = ntest)
}
