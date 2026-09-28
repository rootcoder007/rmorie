.cg_defaults <- list(
  linear = list(A = 1.9, B = -0.06),
  negative_exponential = list(A = 1.89, B = 0.06),
  normal = list(A = 29.5, mean = 4.2, sd = 4.6),
  lognormal = list(A = 8.6, mean = 4.2, sd = 4.6),
  truncated_negative_exponential = list(cutoff = 0.4, peak = 13.8, C = 0.2)
)

.cg_params <- function(fn, params) {
  if (!fn %in% names(.cg_defaults)) {
    stop("function must be linear, negative_exponential, normal, lognormal or truncated_negative_exponential",
         call. = FALSE)
  }
  p <- .cg_defaults[[fn]]
  for (k in names(params)) p[[k]] <- as.numeric(params[[k]])
  p
}

.cg_decay <- function(d, fn, p) {
  switch(fn,
    linear = pmax(0, p$A + p$B * d),
    negative_exponential = p$A * exp(-p$B * d),
    normal = p$A / (p$sd * sqrt(2 * pi)) * exp(-((d - p$mean) / p$sd)^2 / 2),
    lognormal = ifelse(d <= 0, 0, p$A / (d^2 * p$sd * sqrt(2 * pi)) *
                         exp(-(log(pmax(d, 1e-300)^2) - p$mean)^2 / (2 * p$sd^2))),
    truncated_negative_exponential = ifelse(d <= p$cutoff, p$peak / p$cutoff * d,
                                            p$peak * exp(-p$C * (d - p$cutoff))),
    stop("unknown decay function", call. = FALSE)
  )
}

.cg_ols <- function(x, y, intercept = TRUE) {
  if (!intercept) return(c(0, sum(x * y) / sum(x^2)))
  b <- sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)
  c(mean(y) - b * mean(x), b)
}

#' Crime geography: journey to crime, circle hypothesis, hit score and risk terrain
#'
#' \code{JtcDecay}: CrimeStat journey-to-crime decay functions (Levine 2013,
#' equations 13.14-13.20): linear \eqn{\max(0, A + Bd)}, negative
#' exponential \eqn{Ae^{-Bd}}, normal \eqn{A\phi((d-\bar d)/S)/S}, lognormal
#' \eqn{A/(d^2 S\sqrt{2\pi})\exp(-(\ln d^2 - \bar d)^2/(2S^2))} and the
#' truncated negative exponential (linear to the cutoff, exponential beyond),
#' with CrimeStat's Baltimore County defaults. \code{JtcSurface}: the summed
#' likelihood over grid points (Euclidean or Manhattan distance).
#' \code{JtcCalibrate}: grouped-data calibration of all five functions
#' (equations 13.22-13.44) with residual sums of squares.
#' \code{CircleHypothesis}: Canter and Larkin's marauder test.
#' \code{SearchCost}: the hit percentage of a profile. \code{RiskLayers}:
#' proximity or density layers of features around cell centres.
#' \code{RiskTerrain}: Poisson IRLS regression of cell counts on risk layers,
#' relative risk values \eqn{e^{b_k}} and the composite relative risk
#' (Caplan, Kennedy and Miller 2011). Identical to the Python arm
#' \code{morie.fn.crimegeo}.
#'
#' @param d Distances.
#' @param fn Decay function name.
#' @param params Named list overriding the default parameters.
#' @param incidents,crimes Two-column incident coordinates.
#' @param grid Two-column grid coordinates.
#' @param metric \code{"euclidean"} or \code{"manhattan"}.
#' @param distances Known offender journey distances.
#' @param breaks Distance bin breaks.
#' @param home Offender residence (optional).
#' @param scores Profile scores of the search cells.
#' @param home_index Index (1-based) of the home cell.
#' @param cells Two-column cell centres.
#' @param features Two-column feature locations.
#' @param radius Search radius.
#' @param kind \code{"proximity"} or \code{"density"}.
#' @param counts Crime counts per cell.
#' @param layers List (or matrix columns) of risk layers.
#' @param names Layer names.
#' @param tol Relative deviance tolerance.
#' @param maxit Maximum IRLS iterations.
#' @return Numeric vector or list.
#' @references Levine, N. (2013). Journey-to-crime estimation. Chapter 13 in
#'   CrimeStat IV: A Spatial Statistics Program for the Analysis of Crime
#'   Incident Locations, version 4.0. National Institute of Justice (NCJ
#'   242973).
#'
#'   Canter, D. and Larkin, P. (1993). The environmental range of serial
#'   rapists. Journal of Environmental Psychology 13, 63-69.
#'
#'   Rossmo, D. K. (2000). Geographic Profiling. CRC Press.
#'
#'   Caplan, J. M., Kennedy, L. W. and Miller, J. (2011). Risk terrain modeling:
#'   brokering criminological theory and GIS methods for crime forecasting.
#'   Justice Quarterly 28, 360-381.
#' @examples
#' JtcDecay(c(0, 0.4, 1.4), "truncated_negative_exponential")
#' CircleHypothesis(rbind(c(0, 0), c(4, 0), c(2, 1)), home = c(2, -1))$marauder
#' RiskTerrain(c(1, 2, 4, 8), list(c(0, 1, 0, 1), c(0, 0, 1, 1)))$rrv
#' @export
JtcDecay <- function(d, fn = "negative_exponential", params = NULL) {
  p <- .cg_params(fn, params)
  .cg_decay(as.numeric(d), fn, p)
}

#' @rdname JtcDecay
#' @export
JtcSurface <- function(incidents, grid, fn = "negative_exponential", params = NULL, metric = "euclidean") {
  metric <- match.arg(metric, c("euclidean", "manhattan"))
  p <- .cg_params(fn, params)
  X <- matrix(as.numeric(incidents), ncol = 2)
  G <- matrix(as.numeric(grid), ncol = 2)
  s <- vapply(seq_len(nrow(G)), function(g) {
    dx <- G[g, 1] - X[, 1]
    dy <- G[g, 2] - X[, 2]
    d <- if (metric == "euclidean") sqrt(dx^2 + dy^2) else abs(dx) + abs(dy)
    sum(.cg_decay(d, fn, p))
  }, 0)
  k <- which.max(s)
  list(score = s, probability = if (sum(s) > 0) s / sum(s) else rep(NaN, length(s)), peak = G[k, ], peak_index = k)
}

#' @rdname JtcDecay
#' @export
JtcCalibrate <- function(distances, breaks) {
  D <- as.numeric(distances)
  B <- as.numeric(breaks)
  n <- length(D)
  nb <- length(B) - 1
  bin <- findInterval(D, B, rightmost.closed = TRUE)
  cnt <- tabulate(bin[bin >= 1 & bin <= nb], nb)
  pct <- 100 * cnt / n
  mid <- (B[-1] + B[-length(B)]) / 2
  lp <- ifelse(pct > 0, log(pmax(pct, 1e-300)), -16)
  out <- list()
  ab <- .cg_ols(mid, pct)
  out$linear <- list(A = ab[1], B = ab[2])
  kb <- .cg_ols(mid, lp)
  out$negative_exponential <- list(A = exp(kb[1]), B = -kb[2])
  m <- mean(D)
  sd <- stats::sd(D)
  for (fn in c("normal", "lognormal")) {
    ker <- .cg_decay(mid, fn, list(A = 1, mean = m, sd = sd))
    out[[fn]] <- list(A = .cg_ols(ker, pct, FALSE)[2], mean = m, sd = sd)
  }
  j <- which.max(pct)
  beyond <- which(mid > mid[j])
  C <- if (length(beyond) >= 2) -.cg_ols(mid[beyond], lp[beyond])[2] else NaN
  out$truncated_negative_exponential <- list(cutoff = mid[j], peak = pct[j], C = C)
  rss <- vapply(names(out), function(fn) sum((pct - .cg_decay(mid, fn, .cg_params(fn, out[[fn]])))^2), 0)
  list(pct = pct, midpoints = mid, params = out, rss = rss, best = names(rss)[which.min(rss)])
}

#' @rdname JtcDecay
#' @export
CircleHypothesis <- function(crimes, home = NULL) {
  P <- matrix(as.numeric(crimes), ncol = 2)
  D <- as.matrix(stats::dist(P))
  D[lower.tri(D, diag = TRUE)] <- -1
  ij <- which(D == max(D), arr.ind = TRUE)
  ij <- ij[order(ij[, 1], ij[, 2]), , drop = FALSE][1, ]
  cc <- (P[ij[1], ] + P[ij[2], ]) / 2
  r <- sqrt(sum((P[ij[1], ] - P[ij[2], ])^2)) / 2
  tol <- 1e-12 * max(1, r)
  inside <- mean(sqrt((P[, 1] - cc[1])^2 + (P[, 2] - cc[2])^2) <= r + tol)
  mar <- if (is.null(home)) NULL else sqrt(sum((as.numeric(home) - cc)^2)) <= r + tol
  list(center = cc, radius = r, pair = unname(ij), marauder = mar, share_inside = inside)
}

#' @rdname JtcDecay
#' @export
SearchCost <- function(scores, home_index) {
  s <- as.numeric(scores)
  h <- s[home_index]
  above <- sum(s > h)
  ties <- sum(s == h)
  list(hit_percent = 100 * above / length(s), hit_percent_ties = 100 * (above + ties) / length(s), rank = above + 1,
       ties = ties)
}

#' @rdname JtcDecay
#' @export
RiskLayers <- function(cells, features, radius, kind = "proximity") {
  kind <- match.arg(kind, c("proximity", "density"))
  C <- matrix(as.numeric(cells), ncol = 2)
  Fm <- matrix(as.numeric(features), ncol = 2)
  cnt <- vapply(seq_len(nrow(C)), function(i) sum(sqrt((Fm[, 1] - C[i, 1])^2 + (Fm[, 2] - C[i, 2])^2) <= radius), 0)
  if (kind == "proximity") as.numeric(cnt > 0) else cnt
}

#' @rdname JtcDecay
#' @export
RiskTerrain <- function(counts, layers, names = NULL, tol = 1e-10, maxit = 50L) {
  y <- as.numeric(counts)
  L <- if (is.matrix(layers)) layers else do.call(cbind, lapply(layers, as.numeric))
  X <- cbind(1, L)
  beta <- c(log(mean(y)), rep(0, ncol(L)))
  dev_old <- Inf
  for (it in seq_len(maxit)) {
    eta <- drop(X %*% beta)
    mu <- exp(eta)
    z <- eta + (y - mu) / mu
    beta <- drop(solve(crossprod(X, X * mu), crossprod(X, mu * z)))
    mu <- exp(drop(X %*% beta))
    dev <- 2 * sum(ifelse(y > 0, y * log(pmax(y, 1e-300) / mu), 0) - (y - mu))
    if (abs(dev - dev_old) / (abs(dev) + 0.1) < tol) break
    dev_old <- dev
  }
  se <- sqrt(diag(solve(crossprod(X, X * mu))))
  rel <- exp(drop(L %*% beta[-1]))
  z <- beta / se
  list(names = if (is.null(names)) paste0("layer", seq_len(ncol(L))) else names, coefficients = unname(beta),
       se = unname(se), z = unname(z), pvalue = unname(2 * (1 - stats::pnorm(abs(z)))), rrv = unname(exp(beta[-1])),
       relative_risk = rel, risk_score = rel / min(rel), fitted = mu, deviance = dev)
}
