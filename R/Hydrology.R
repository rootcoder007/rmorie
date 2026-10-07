.hy_eu <- 0.57721566490153286

.hy_pelgev <- function(l1, l2, t3) {
  if (t3 > 0) {
    z <- 1 - t3
    g <- (-1 + z * (1.59921491 + z * (-0.48832213 + z * 0.01573152))) / (1 + z * (-0.64363929 + z * 0.08985247))
    if (abs(g) < 1e-5) {
      a <- l2 / log(2)
      return(c(l1 - .hy_eu * a, a, 0))
    }
  } else {
    g <- (0.28377530 + t3 * (-1.21096399 + t3 * (-2.50728214 + t3 * (-1.13455566 + t3 * -0.07138022)))) /
      (1 + t3 * (2.06189696 + t3 * (1.31912239 + t3 * 0.25077104)))
    if (t3 < -0.8) {
      if (t3 <= -0.97) g <- 1 - log(1 + t3) / log(2)
      t0 <- (t3 + 3) * 0.5
      for (it in 1:20) {
        x2 <- 2^(-g)
        x3 <- 3^(-g)
        deriv <- ((1 - x2) * x3 * log(3) - (1 - x3) * x2 * log(2)) / (1 - x2)^2
        gold <- g
        g <- g - ((1 - x3) / (1 - x2) - t0) / deriv
        if (abs(g - gold) <= 1e-6 * g) break
      }
    }
  }
  gam <- exp(lgamma(1 + g))
  a <- l2 * g / (gam * (1 - 2^(-g)))
  c(l1 - a * (1 - gam) / g, a, g)
}

.hy_quape3 <- function(f, mu, sd, g) {
  if (abs(g) <= 1e-8) return(stats::qnorm(f, mu, sd))
  al <- 4 / g^2
  be <- abs(0.5 * sd * g)
  if (g > 0) mu - al * be + be * pmax(0, stats::qgamma(f, al)) else mu + al * be - be * pmax(0, stats::qgamma(1 - f, al))
}

.hy_moments <- function(v) {
  n <- length(v)
  m <- mean(v)
  s <- stats::sd(v)
  c(m, s, n * sum((v - m)^3) / ((n - 1) * (n - 2) * s^3))
}

#' Flood, low-flow and flow-duration frequency analysis
#'
#' \code{SampleLmoments}: \eqn{l_1, l_2, t_3, t_4} from unbiased
#' probability-weighted moments (Hosking 1990), as \code{lmom::samlmu}.
#' \code{FloodFrequency}: T-year floods \eqn{F^{-1}(1 - 1/T)} of annual
#' maxima by GEV or Gumbel L-moment fits (GEV shape by Hosking's rational
#' approximations, as \code{lmom::pelgev}) or Bulletin 17B log-Pearson III
#' (moments of \eqn{\log_{10} Q}, bias-corrected skew).
#' \code{LowFlowFrequency}: the \code{d}-day, \code{T}-year low flow from
#' annual minima of the \code{d}-day moving mean, by the three-parameter
#' Weibull (L-moments, as \code{lmom::pelwei}) or log-Pearson III.
#' \code{FlowDurationCurve}: descending flows against exceedance
#' \eqn{i / (n + 1)}, with interpolated quantiles. Identical to the Python arm
#' \code{morie.fn.hydro}.
#'
#' @param x,amax,flows Data.
#' @param dist Distribution.
#' @param return_periods Return periods in years.
#' @param years Year of every daily flow.
#' @param d Averaging window in days.
#' @param T Return period.
#' @param probs Exceedance probabilities.
#' @return List (numeric vector for \code{SampleLmoments}).
#' @references Hosking, J. R. M. and Wallis, J. R. (1997). Regional
#'   Frequency Analysis: An Approach Based on L-Moments. Cambridge University
#'   Press.
#'
#'   Interagency Advisory Committee on Water Data (1982). Guidelines for
#'   Determining Flood Flow Frequency, Bulletin 17B. USGS.
#'
#'   Vogel, R. M. and Fennessey, N. M. (1994). Flow-duration curves. I: New
#'   interpretation and confidence intervals. Journal of Water Resources
#'   Planning and Management 120, 485-504.
#' @examples
#' SampleLmoments(c(1, 2, 3, 4, 10))
#' FloodFrequency(c(120, 95, 310, 180, 150, 220, 90, 260, 140, 175), dist = "gev")$quantiles
#' FlowDurationCurve(c(3, 1, 2, 4), 0.5)$quantiles
#' @export
SampleLmoments <- function(x) {
  s <- sort(as.numeric(x))
  n <- length(s)
  i <- seq_len(n) - 1
  b <- c(mean(s), vapply(1:3, function(r) {
    w <- rep(1, n)
    for (k in 0:(r - 1)) w <- w * (i - k) / (n - 1 - k)
    sum(s * w) / n
  }, 0))
  l2 <- 2 * b[2] - b[1]
  c(b[1], l2, (6 * b[3] - 6 * b[2] + b[1]) / l2, (20 * b[4] - 30 * b[3] + 12 * b[2] - b[1]) / l2)
}

#' @rdname SampleLmoments
#' @export
FloodFrequency <- function(amax, dist = "gev", return_periods = c(2, 5, 10, 25, 50, 100)) {
  q <- as.numeric(amax)
  f <- 1 - 1 / return_periods
  if (dist %in% c("gev", "gumbel")) {
    lm <- SampleLmoments(q)
    if (dist == "gumbel") {
      a <- lm[2] / log(2)
      par <- c(lm[1] - .hy_eu * a, a)
      qs <- par[1] - par[2] * log(-log(f))
    } else {
      par <- .hy_pelgev(lm[1], lm[2], lm[3])
      qs <- if (par[3] == 0) par[1] - par[2] * log(-log(f)) else par[1] + par[2] / par[3] * (1 - (-log(f))^par[3])
    }
  } else if (dist == "lp3") {
    par <- .hy_moments(log10(q))
    qs <- 10^vapply(f, function(p) .hy_quape3(p, par[1], par[2], par[3]), 0)
  } else {
    stop("dist must be gev, gumbel or lp3", call. = FALSE)
  }
  list(params = par, return_periods = return_periods, quantiles = qs, dist = dist)
}

#' @rdname SampleLmoments
#' @export
LowFlowFrequency <- function(flows, years, d = 7L, T = 10, dist = "weibull") {
  q <- as.numeric(flows)
  mins <- numeric(0)
  for (yv in unique(years)) {
    s <- q[years == yv]
    if (length(s) >= d) mins <- c(mins, min(vapply(seq_len(length(s) - d + 1), function(i) mean(s[i:(i + d - 1)]), 0)))
  }
  f <- 1 / T
  if (dist == "weibull") {
    lm <- SampleLmoments(mins)
    pg <- .hy_pelgev(-lm[1], lm[2], -lm[3])
    par <- c(-pg[1] - pg[2] / pg[3], pg[2] / pg[3], 1 / pg[3])
    val <- par[1] + par[2] * (-log(1 - f))^(1 / par[3])
  } else if (dist == "lp3") {
    par <- .hy_moments(log10(mins))
    val <- 10^.hy_quape3(f, par[1], par[2], par[3])
  } else {
    stop("dist must be weibull or lp3", call. = FALSE)
  }
  list(annual_minima = mins, params = par, quantile = val, d = d, T = T)
}

#' @rdname SampleLmoments
#' @export
FlowDurationCurve <- function(flows, probs = NULL) {
  s <- sort(as.numeric(flows), decreasing = TRUE)
  n <- length(s)
  p <- seq_len(n) / (n + 1)
  qs <- vapply(probs, function(pr) {
    if (pr <= p[1]) return(s[1])
    if (pr >= p[n]) return(s[n])
    k <- max(which(p <= pr))
    if (k == n) return(s[n])
    s[k] + (pr - p[k]) / (p[k + 1] - p[k]) * (s[k + 1] - s[k])
  }, 0)
  list(flows = s, exceedance = p, quantiles = qs)
}

#' Rainfall-runoff: IDF curves, unit hydrographs, baseflow and hydrograph summaries
#'
#' \code{IdfFit}: Sherman curve \eqn{i = a / (t + b)^c} by Levenberg-Marquardt
#' from the log-linear \eqn{b = 0} start. \code{UnitHydrograph}: D-hour unit
#' hydrograph (m^3/s per cm over \code{area} km^2): SCS triangular
#' (\eqn{T_p = D/2 + 0.6 t_c}, \eqn{q_p = 2.08 A / T_p}, base
#' \eqn{2.67 T_p}) or the Nash cascade S-curve difference.
#' \code{ConvolveRunoff}: discrete convolution of excess rain with UH
#' ordinates. \code{BaseflowFilter}: Lyne-Hollick filter, forward/backward
#' passes with quickflow clamped between 0 and the flow, and the baseflow index.
#' \code{HydrographSummary}: peak, time to peak, trapezoidal volume, BFI and
#' the pooled log-linear recession constant. Identical to the Python arm
#' \code{morie.fn.hydro}.
#'
#' @param durations,intensities Durations and rainfall intensities.
#' @param start Optional \code{c(a, b, c)}.
#' @param tol,maxit Relative parameter-step tolerance and iteration limit.
#' @param method \code{"scs"} or \code{"nash"}.
#' @param area Catchment area, km^2.
#' @param dt Time step, hours.
#' @param D Excess-rain duration, hours.
#' @param tc Time of concentration, hours.
#' @param n,k Nash reservoirs and storage constant (hours).
#' @param excess,uh Excess rain depths and UH ordinates.
#' @param flows Flow series.
#' @param alpha Filter parameter.
#' @param passes Number of filter passes.
#' @param min_recession Minimum recession run length.
#' @return List (vector for \code{ConvolveRunoff}).
#' @references Sherman, C. W. (1931). Frequency and intensity of excessive
#'   rainfalls at Boston, Massachusetts. Transactions of the ASCE 95, 951-960.
#'
#'   Nash, J. E. (1957). The form of the instantaneous unit hydrograph. IAHS
#'   Publication 45(3), 114-121.
#'
#'   Nathan, R. J. and McMahon, T. A. (1990). Evaluation of automated
#'   techniques for base flow and recession analyses. Water Resources
#'   Research 26, 1465-1473.
#' @examples
#' t <- c(5, 10, 15, 30, 60, 120)
#' IdfFit(t, 1000 / (t + 8)^0.7)[c("a", "b", "c")]
#' UnitHydrograph("scs", area = 10, dt = 1, D = 2, tc = 5 / 0.6)$peak
#' ConvolveRunoff(c(1, 2), c(0, 1, 3, 1, 0))
#' HydrographSummary(c(1, 4, 8, 4, 2, 1, 0.5))$recession_constant
#' @export
IdfFit <- function(durations, intensities, start = NULL, tol = 1e-13, maxit = 500L) {
  t <- as.numeric(durations)
  y <- as.numeric(intensities)
  if (is.null(start)) {
    lx <- log(t)
    ly <- log(y)
    cc <- -sum((lx - mean(lx)) * (ly - mean(ly))) / sum((lx - mean(lx))^2)
    par <- c(exp(mean(ly) + cc * mean(lx)), 0, cc)
  } else {
    par <- as.numeric(start)
  }
  rss <- function(p) if (min(t + p[2]) <= 0) Inf else sum((y - p[1] / (t + p[2])^p[3])^2)
  lam <- 1e-3
  cur <- rss(par)
  for (it in seq_len(maxit)) {
    u <- t + par[2]
    f <- par[1] / u^par[3]
    J <- cbind(f / par[1], -par[3] * f / u, -f * log(u))
    r <- y - f
    JtJ <- crossprod(J)
    Jtr <- as.vector(crossprod(J, r))
    improved <- FALSE
    for (i in 1:60) {
      M <- JtJ
      diag(M) <- diag(M) * (1 + lam)
      step <- as.vector(solve(M) %*% Jtr)
      cand <- par + step
      new <- rss(cand)
      if (new <= cur) {
        improved <- TRUE
        break
      }
      lam <- lam * 10
    }
    if (!improved) break
    done <- all(abs(step) <= tol * abs(cand))
    par <- cand
    cur <- new
    lam <- max(lam / 10, 1e-12)
    if (done) break
  }
  list(a = par[1], b = par[2], c = par[3], rss = cur, fitted = par[1] / (t + par[2])^par[3])
}

#' @rdname IdfFit
#' @export
UnitHydrograph <- function(method = "scs", area = 1, dt = 0.5, D = 1, tc = NULL, n = NULL, k = NULL) {
  if (method == "scs") {
    if (is.null(tc)) stop("scs needs tc", call. = FALSE)
    tp <- D / 2 + 0.6 * tc
    qp <- 2.08 * area / tp
    tb <- 2.67 * tp
    times <- (0:ceiling(tb / dt)) * dt
    q <- ifelse(times <= tp, qp * times / tp, ifelse(times < tb, qp * (tb - times) / (tb - tp), 0))
    return(list(time = times, ordinates = q, time_to_peak = tp, peak = qp, base = tb))
  }
  if (method != "nash") stop("method must be scs or nash", call. = FALSE)
  if (is.null(n) || is.null(k)) stop("nash needs n and k", call. = FALSE)
  G <- function(t) if (t > 0) stats::pgamma(t / k, n) else 0
  times <- 0
  q <- 0
  i <- 1
  repeat {
    t <- i * dt
    times <- c(times, t)
    q <- c(q, area / 0.36 * (G(t) - G(t - D)) / D)
    if (t > D && G(t - D) > 1 - 1e-10) break
    i <- i + 1
  }
  j <- which.max(q)
  list(time = times, ordinates = q, time_to_peak = times[j], peak = q[j], base = times[length(times)])
}

#' @rdname IdfFit
#' @export
ConvolveRunoff <- function(excess, uh) {
  P <- as.numeric(excess)
  U <- as.numeric(uh)
  vapply(seq_len(length(P) + length(U) - 1), function(j) {
    m <- seq_along(P)
    ok <- j - m + 1 >= 1 & j - m + 1 <= length(U)
    sum(P[m[ok]] * U[j - m[ok] + 1])
  }, 0)
}

#' @rdname IdfFit
#' @export
BaseflowFilter <- function(flows, alpha = 0.925, passes = 3L) {
  q <- as.numeric(flows)
  b <- q
  for (ps in seq_len(passes) - 1) {
    s <- if (ps %% 2 == 0) b else rev(b)
    out <- s
    f <- 0
    for (t in seq_along(s)[-1]) {
      f <- alpha * f + (1 + alpha) / 2 * (s[t] - s[t - 1])
      f <- min(max(f, 0), s[t])
      out[t] <- s[t] - f
    }
    b <- if (ps %% 2 == 0) out else rev(out)
  }
  list(baseflow = b, quickflow = q - b, bfi = sum(b) / sum(q))
}

#' @rdname IdfFit
#' @export
HydrographSummary <- function(flows, dt = 1, alpha = 0.925, min_recession = 3L) {
  q <- as.numeric(flows)
  j <- which.max(q)
  runs <- list()
  cur <- 1L
  for (t in seq_along(q)[-1]) {
    if (q[t] < q[t - 1] && q[t] > 0) {
      cur <- c(cur, t)
    } else {
      if (length(cur) > min_recession) runs[[length(runs) + 1]] <- cur
      cur <- t
    }
  }
  if (length(cur) > min_recession) runs[[length(runs) + 1]] <- cur
  sxy <- sum(vapply(runs, function(r) sum((r - mean(r)) * (log(q[r]) - mean(log(q[r])))), 0))
  sxx <- sum(vapply(runs, function(r) sum((r - mean(r))^2), 0))
  list(peak = q[j], time_to_peak = (j - 1) * dt, volume = dt * (sum(q) - 0.5 * (q[1] + q[length(q)])),
       bfi = BaseflowFilter(q, alpha)$bfi, recession_constant = if (sxx > 0) exp(sxy / sxx) else NaN,
       n_recessions = length(runs))
}

#' Stream networks, channel form, groundwater flow and depression breaching
#'
#' \code{StreamSegments}: channel cells (accumulation at least
#' \code{threshold}), Strahler segments with D8 lengths (including the step
#' into the receiver) and Horton's bifurcation and length ratios from the
#' log-linear fits of segment counts and mean lengths on order.
#' \code{ChannelSlope}: simple, 10-85, equal-area and harmonic
#' (Taylor-Schwarz) slopes of a long profile. \code{MeanderMetrics}:
#' sinuosity, three-point curvature and radius, inflections and mean
#' wavelength. \code{DarcyFlow}: \eqn{q = -K \nabla h} by central
#' differences (x east, y north) and seepage velocity.
#' \code{BreachDepressions}: priority flood that lowers the path back to the
#' edge instead of filling (Soille 2004). \code{HeightAboveDrainage}: HAND
#' and flow distance along D8 paths to the first channel cell, with
#' floodplain and riparian masks (Renno et al. 2008). Identical to the Python
#' arm \code{morie.fn.hydro}; segment cells are 1-based linear indices here.
#'
#' @param flowdir D8 codes (as \code{D8FlowDirection}).
#' @param acc Flow accumulation.
#' @param threshold Channel initiation threshold.
#' @param res Cell size.
#' @param distance,elevation Long profile, distance from the outlet.
#' @param x,y Centreline coordinates.
#' @param head Hydraulic head grid.
#' @param K Hydraulic conductivity (scalar or grid).
#' @param porosity Effective porosity.
#' @param dem Elevation grid.
#' @param epsilon Minimum drop along carved paths.
#' @param channel Channel mask.
#' @param max_height,max_distance Floodplain and riparian thresholds.
#' @return List (matrix for \code{BreachDepressions}).
#' @references Horton, R. E. (1945). Erosional development of streams and
#'   their drainage basins. GSA Bulletin 56, 275-370.
#'
#'   Soille, P. (2004). Optimal removal of spurious pits in grid digital
#'   elevation models. Water Resources Research 40, W12509.
#'
#'   Renno, C. D. et al. (2008). HAND, a new terrain descriptor using
#'   SRTM-DEM. Remote Sensing of Environment 112, 3469-3481.
#' @examples
#' fd <- rbind(c(2, 4, 8), c(1, 4, 16), c(1, 4, 16))
#' StreamSegments(fd, FlowAccumulation(fd), 1)$counts
#' ChannelSlope(c(0, 100, 200), c(10, 11, 14))$equal_area
#' MeanderMetrics(c(0, 1, 2), c(0, 1, 0))$sinuosity
#' DarcyFlow(rbind(c(10, 9, 8), c(10, 9, 8)), 2, res = 10)$qx
#' BreachDepressions(rbind(c(5, 5, 5, 5), c(5, 1, 3, 5), c(5, 5, 2, 5), c(5, 5, 0, 5)))
#' HeightAboveDrainage(rbind(c(3, 2, 1)), rbind(c(1, 1, 0)), rbind(c(0, 0, 1)))$hand
#' @export
StreamSegments <- function(flowdir, acc, threshold, res = 1) {
  fd <- as.matrix(flowdir)
  nr <- nrow(fd)
  ord <- StreamOrder(fd, acc, threshold)
  rec <- .dem_receiver(fd)
  on <- as.vector(ord)
  step <- function(c, d) {
    a <- (d - 1) %% nr - (c - 1) %% nr
    b <- (d - 1) %/% nr - (c - 1) %/% nr
    res * (if (a != 0 && b != 0) sqrt(2) else 1)
  }
  segs <- list()
  for (i in seq_len(nr)) {
    for (j in seq_len(ncol(fd))) {
      c0 <- (j - 1) * nr + i
      w <- on[c0]
      if (!w) next
      dn <- which(!is.na(rec) & rec == c0 & on > 0)
      if (any(on[dn] == w)) next
      cells <- c0
      L <- 0
      cc <- c0
      repeat {
        d <- rec[cc]
        if (is.na(d) || !on[d]) break
        L <- L + step(cc, d)
        if (on[d] != w) break
        cells <- c(cells, d)
        cc <- d
      }
      segs[[length(segs) + 1]] <- list(order = w, cells = cells, length = L)
    }
  }
  W <- max(c(0, vapply(segs, `[[`, 0, "order")))
  ords <- vapply(segs, `[[`, 0, "order")
  lens <- vapply(segs, `[[`, 0, "length")
  counts <- vapply(seq_len(W), function(w) sum(ords == w), 0L)
  mlen <- vapply(seq_len(W), function(w) if (counts[w]) mean(lens[ords == w]) else 0, 0)
  slope <- function(v) {
    ok <- v > 0
    if (sum(ok) < 2) return(NaN)
    w <- seq_along(v)[ok]
    lv <- log10(v[ok])
    sum((w - mean(w)) * (lv - mean(lv))) / sum((w - mean(w))^2)
  }
  list(order = ord, channel = (ord > 0) * 1L, segments = segs, counts = counts, mean_lengths = mlen,
       bifurcation_ratio = 10^-slope(counts), length_ratio = 10^slope(mlen))
}

#' @rdname StreamSegments
#' @export
ChannelSlope <- function(distance, elevation) {
  x <- as.numeric(distance)
  z <- as.numeric(elevation)
  L <- x[length(x)] - x[1]
  at <- function(u) stats::approx(x, z, u)$y
  m <- length(x)
  A <- sum(0.5 * ((z[-m] - z[1]) + (z[-1] - z[1])) * diff(x))
  sl <- diff(z) / diff(x)
  li <- diff(x)
  pos <- sl > 0
  harm <- if (any(pos)) (sum(li[pos]) / sum(li[pos] / sqrt(sl[pos])))^2 else 0
  list(simple = (z[m] - z[1]) / L, s1085 = (at(x[1] + 0.85 * L) - at(x[1] + 0.10 * L)) / (0.75 * L),
       equal_area = 2 * A / L^2, harmonic = harm, length = L)
}

#' @rdname StreamSegments
#' @export
MeanderMetrics <- function(x, y) {
  P <- cbind(as.numeric(x), as.numeric(y))
  n <- nrow(P)
  dd <- function(a, b) sqrt(sum((P[a, ] - P[b, ])^2))
  seg <- vapply(seq_len(n - 1), function(i) dd(i, i + 1), 0)
  curv <- vapply(seq_len(n - 2) + 1, function(i) {
    cr <- (P[i, 1] - P[i - 1, 1]) * (P[i + 1, 2] - P[i, 2]) - (P[i, 2] - P[i - 1, 2]) * (P[i + 1, 1] - P[i, 1])
    2 * cr / (seg[i - 1] * seg[i] * dd(i - 1, i + 1))
  }, 0)
  infl <- which(curv[-length(curv)] * curv[-1] < 0) + 1
  wl <- if (length(infl) > 1) 2 * vapply(seq_len(length(infl) - 1), function(k) dd(infl[k], infl[k + 1]), 0) else numeric(0)
  list(length = sum(seg), sinuosity = sum(seg) / dd(1, n), curvature = curv,
       radius = ifelse(curv != 0, 1 / abs(curv), Inf), inflections = infl,
       wavelength = if (length(wl)) mean(wl) else NaN)
}

#' @rdname StreamSegments
#' @export
DarcyFlow <- function(head, K, res = 1, porosity = NULL) {
  H <- as.matrix(head) * 1
  nr <- nrow(H)
  nc <- ncol(H)
  Kg <- if (length(K) == 1) matrix(K, nr, nc) else as.matrix(K)
  qx <- matrix(0, nr, nc)
  qy <- matrix(0, nr, nc)
  for (i in seq_len(nr)) {
    for (j in seq_len(nc)) {
      if (nc > 1) {
        lo <- max(j - 1, 1)
        hi <- min(j + 1, nc)
        qx[i, j] <- -Kg[i, j] * (H[i, hi] - H[i, lo]) / ((hi - lo) * res)
      }
      if (nr > 1) {
        lo <- max(i - 1, 1)
        hi <- min(i + 1, nr)
        qy[i, j] <- -Kg[i, j] * -((H[hi, j] - H[lo, j]) / ((hi - lo) * res))
      }
    }
  }
  out <- list(qx = qx, qy = qy, magnitude = sqrt(qx^2 + qy^2))
  if (!is.null(porosity)) out$velocity <- out$magnitude / porosity
  out
}

#' @rdname StreamSegments
#' @export
BreachDepressions <- function(dem, epsilon = 0) {
  .morie_arg(dem, "m")
  Z <- as.matrix(dem) * 1
  G <- Z
  nr <- nrow(Z)
  nc <- ncol(Z)
  done <- is.na(Z)
  parent <- rep(NA_integer_, nr * nc)
  pq <- matrix(numeric(0), 0, 3)
  for (i in seq_len(nr)) {
    for (j in seq_len(nc)) {
      if (is.na(Z[i, j])) next
      edge <- i == 1 || i == nr || j == 1 || j == nc
      if (!edge) for (k in 1:8) if (is.na(G[i + .dem_off[k, 1], j + .dem_off[k, 2]])) edge <- TRUE
      if (edge) {
        pq <- rbind(pq, c(Z[i, j], i, j))
        done[i, j] <- TRUE
      }
    }
  }
  while (nrow(pq)) {
    m <- order(pq[, 1], pq[, 2], pq[, 3])[1]
    i <- pq[m, 2]
    j <- pq[m, 3]
    pq <- pq[-m, , drop = FALSE]
    for (k in 1:8) {
      x <- i + .dem_off[k, 1]
      y <- j + .dem_off[k, 2]
      if (x >= 1 && x <= nr && y >= 1 && y <= nc && !done[x, y]) {
        done[x, y] <- TRUE
        parent[(y - 1) * nr + x] <- (j - 1) * nr + i
        cz <- Z[x, y]
        p <- (j - 1) * nr + i
        while (!is.na(p) && Z[p] > cz - epsilon) {
          Z[p] <- cz - epsilon
          cz <- Z[p]
          p <- parent[p]
        }
        pq <- rbind(pq, c(Z[x, y], x, y))
      }
    }
  }
  Z
}

#' @rdname StreamSegments
#' @export
HeightAboveDrainage <- function(dem, flowdir, channel, res = 1, max_height = NULL, max_distance = NULL) {
  Z <- as.matrix(dem) * 1
  fd <- as.matrix(flowdir)
  ch <- as.vector(as.matrix(channel)) != 0
  nr <- nrow(Z)
  rec <- .dem_receiver(fd)
  N <- length(Z)
  tgt <- rep(NA_integer_, N)
  L <- rep(NaN, N)
  known <- rep(FALSE, N)
  for (c0 in seq_len(N)) {
    if (is.na(Z[c0]) || known[c0]) next
    path <- integer(0)
    cc <- c0
    repeat {
      if (known[cc]) break
      if (ch[cc]) {
        tgt[cc] <- cc
        L[cc] <- 0
        known[cc] <- TRUE
        break
      }
      path <- c(path, cc)
      d <- rec[cc]
      if (is.na(d) || d %in% path) {
        known[cc] <- TRUE
        break
      }
      cc <- d
    }
    for (p in rev(path)) {
      d <- rec[p]
      known[p] <- TRUE
      if (is.na(d) || is.na(tgt[d])) next
      a <- (d - 1) %% nr - (p - 1) %% nr
      b <- (d - 1) %/% nr - (p - 1) %/% nr
      tgt[p] <- tgt[d]
      L[p] <- L[d] + res * (if (a != 0 && b != 0) sqrt(2) else 1)
    }
  }
  hand <- matrix(ifelse(is.na(tgt), NaN, Z - Z[pmax(tgt, 1)]), nr)
  dist <- matrix(ifelse(is.na(tgt), NaN, L), nr)
  out <- list(hand = hand, distance = dist)
  if (!is.null(max_height)) {
    out$floodplain <- (!is.na(hand) & hand <= max_height) * 1L
    if (!is.null(max_distance)) out$riparian <- (!is.na(hand) & hand <= max_height & dist <= max_distance) * 1L
  }
  out
}
