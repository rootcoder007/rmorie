.scan_nn <- function(P, pop, ubpop) {
  D <- as.matrix(stats::dist(P))
  tp <- sum(pop)
  lapply(seq_len(nrow(P)), function(i) {
    o <- order(D[i, ])
    o[cumsum(pop[o]) <= tp * ubpop]
  })
}

.scan_llr <- function(yin, ty, ein, popin, tpop, kind, min_cases) {
  if (yin < min_cases) return(0)
  yout <- ty - yin
  if (kind == "poisson") {
    eout <- ty - ein
    if (yin / ein <= (if (eout > 0) yout / eout else Inf)) return(0)
    return(yin * (log(yin) - log(ein)) + if (yout > 0) yout * (log(yout) - log(eout)) else 0)
  }
  popout <- tpop - popin
  if (popout <= 0 || yin / popin <= yout / popout) return(0)
  xl <- function(a, b) if (a > 0) a * log(b) else 0
  t <- xl(yin, yin) - xl(yin, popin) + xl(popin - yin, popin - yin) - xl(popin - yin, popin) + xl(yout, yout) -
    xl(yout, popout) + xl(popout - yout, popout - yout) - xl(popout - yout, popout) - xl(ty, ty) -
    xl(tpop - ty, tpop - ty) + xl(tpop, tpop)
  if (is.nan(t)) 0 else t
}

.scan_stats <- function(nn, y, ex, pop, kind, min_cases) {
  ty <- sum(y)
  mult <- ty / sum(ex)
  lapply(nn, function(l) {
    yin <- cumsum(y[l])
    ein <- cumsum(ex[l]) * mult
    pin <- cumsum(pop[l])
    vapply(seq_along(l), function(t) .scan_llr(yin[t], ty, ein[t], pin[t], sum(pop), kind, min_cases), 0)
  })
}

.scan_noc <- function(nn, st) {
  remaining <- which(lengths(nn) > 0)
  clusts <- list()
  tobs <- numeric(0)
  cur <- 1
  while (length(remaining) && cur > 0) {
    ok <- remaining[lengths(st[remaining]) > 0]
    if (!length(ok)) break
    mx <- vapply(ok, function(i) max(st[[i]]), 0)
    bi <- ok[which.max(mx)]
    bk <- which.max(st[[bi]])
    clusts <- c(clusts, list(nn[[bi]][seq_len(bk)]))
    cur <- max(mx)
    tobs <- c(tobs, cur)
    used <- unlist(clusts)
    remaining <- setdiff(remaining, used)
    for (i in remaining) {
      w <- which(nn[[i]] %in% used)
      if (length(w)) st[[i]] <- st[[i]][seq_len(w[1] - 1)]
    }
  }
  list(zones = clusts, tobs = tobs)
}

.scan_multinom <- function(total, prob, seed, stream) {
  cum <- cumsum(prob / sum(prob))
  cum[length(cum)] <- 1
  u <- .morie_random_uniform(total, seed = seed, stream = stream)
  tabulate(findInterval(u, cum) + 1, length(prob))
}

#' Spatial cluster detection: circular scan, Besag-Newell, Tango and Stone
#'
#' \code{ScanZones}: distinct nearest-neighbour zones within \code{ubpop} of
#' the population (\code{smerc::scan.zones}). \code{KulldorffScan}: Poisson or
#' Bernoulli likelihood-ratio scan with non-overlapping secondary clusters
#' and Monte Carlo p-values from Philox multinomial redistributions (Kulldorff
#' 1997; \code{smerc::scan.test}). \code{BesagNewell}: nearest regions pooled
#' to \code{k} cases, \eqn{P(Poisson(E) \ge k_{obs})} (Besag and Newell 1991;
#' SpatialEpi). \code{TangoTest}: Tango's (1995) index with its chi-square
#' approximation (\code{smerc::tango.test}). \code{StoneTest}: Stone's (1988)
#' maximum cumulative O/E by distance order. Identical to the Python arm
#' \code{morie.fn.scanstat}.
#'
#' @param coords Coordinates.
#' @param cases Counts.
#' @param pop Populations.
#' @param ubpop Population upper bound (fraction).
#' @param ex Expected counts.
#' @param kind \code{"poisson"} or \code{"binomial"}.
#' @param nsim Monte Carlo replications.
#' @param alpha Significance level for reported clusters.
#' @param min_cases Minimum cases in a zone.
#' @param seed Philox seed.
#' @param k Case threshold.
#' @param expected Expected counts.
#' @param W Tango weight matrix.
#' @param order Region order by distance from the source (1-based).
#' @return List.
#' @references Kulldorff, M. (1997). A spatial scan statistic. Communications
#'   in Statistics - Theory and Methods 26, 1481-1496.
#'
#'   Tango, T. (1995). A class of tests for detecting general and focused
#'   clustering of rare diseases. Statistics in Medicine 14, 2323-2334.
#' @examples
#' P <- cbind(0:5, 0)
#' KulldorffScan(P, c(9, 8, 1, 1, 1, 0), rep(10, 6), nsim = 0)$all_tobs
#' BesagNewell(cbind(c(0, 1, 2, 9), 0), c(3, 2, 0, 1), rep(10, 4), 4)$m_values
#' @export
ScanZones <- function(coords, pop, ubpop = 0.5) {
  nn <- .scan_nn(as.matrix(coords), pop, ubpop)
  z <- unlist(lapply(nn, function(l) lapply(seq_along(l), function(t) l[seq_len(t)])), recursive = FALSE)
  z[!duplicated(lapply(z, sort))]
}

#' @rdname ScanZones
#' @export
KulldorffScan <- function(coords, cases, pop, ex = NULL, kind = "poisson", ubpop = 0.5, nsim = 499L, alpha = 0.1,
                          min_cases = 2, seed = 1L) {
  if (!kind %in% c("poisson", "binomial")) stop("kind must be poisson or binomial")
  e <- if (is.null(ex)) pop * sum(cases) / sum(pop) else ex
  nn <- .scan_nn(as.matrix(coords), pop, ubpop)
  r <- .scan_noc(nn, .scan_stats(nn, cases, e, pop, kind, min_cases))
  pv <- rep(1, length(r$tobs))
  if (nsim > 0) {
    tmax <- vapply(seq_len(nsim), function(s) {
      ys <- .scan_multinom(sum(cases), e, seed, s)
      max(unlist(.scan_stats(nn, ys, e, pop, kind, min_cases)), 0)
    }, 0)
    pv <- vapply(r$tobs, function(x) (1 + sum(tmax >= x)) / (nsim + 1), 0)
  }
  sig <- which(pv <= alpha)
  if (!length(sig)) sig <- which.min(pv)
  clusters <- lapply(sig, function(i) {
    list(zone = r$zones[[i]], llr = r$tobs[i], pvalue = pv[i], cases = sum(cases[r$zones[[i]]]),
         expected = sum(e[r$zones[[i]]]) * sum(cases) / sum(e))
  })
  list(clusters = clusters, all_zones = r$zones, all_tobs = r$tobs, all_pvalues = pv)
}

#' @rdname ScanZones
#' @export
BesagNewell <- function(coords, cases, pop, k, expected = NULL) {
  D <- as.matrix(stats::dist(as.matrix(coords)))
  E <- if (is.null(expected)) pop * sum(cases) / sum(pop) else expected
  res <- t(vapply(seq_along(cases), function(i) {
    o <- order(D[i, ])
    cc <- cumsum(cases[o])
    m <- which(cc >= k)[1]
    if (is.na(m)) return(c(length(o), cc[length(o)], 1))
    c(m, cc[m], 1 - stats::ppois(cc[m] - 1, sum(E[o[seq_len(m)]])))
  }, numeric(3)))
  list(m_values = res[, 1], k_values = res[, 2], p_values = res[, 3])
}

#' @rdname ScanZones
#' @export
TangoTest <- function(cases, pop, W) {
  W <- as.matrix(W)
  N <- length(cases)
  Y <- sum(cases)
  ee <- cases / Y - pop / sum(pop)
  p <- pop / sum(pop)
  gof <- sum(ee^2)
  sa <- as.numeric(t(ee) %*% (W - diag(N)) %*% ee)
  WV <- W %*% (diag(p) - tcrossprod(p))
  WV2 <- WV %*% WV
  tr2 <- sum(diag(WV2))
  ec <- sum(diag(WV)) / Y
  vc <- 2 * tr2 / Y^2
  skc <- 2 * sqrt(2) * sum(diag(WV2 %*% WV)) / tr2^1.5
  dfc <- 8 / skc^2
  t <- gof + sa
  tchi <- dfc + (t - ec) / sqrt(vc) * sqrt(2 * dfc)
  list(tstat = t, gof = gof, sa = sa, tstat_chisq = tchi, dfc = dfc, pvalue_chisq = 1 - stats::pchisq(tchi, dfc))
}

#' @rdname ScanZones
#' @export
StoneTest <- function(cases, expected, order, nsim = 0L, seed = 1L) {
  stat <- function(v) {
    r <- cumsum(v[order]) / cumsum(expected[order])
    c(max(r), which.max(r))
  }
  s <- stat(cases)
  out <- list(statistic = s[1], k = s[2])
  if (nsim > 0) {
    sims <- vapply(seq_len(nsim), function(i) stat(.scan_multinom(sum(cases), expected, seed, i))[1], 0)
    out$pvalue <- (1 + sum(sims >= s[1])) / (nsim + 1)
  }
  out
}
