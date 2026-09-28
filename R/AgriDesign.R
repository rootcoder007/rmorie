# SPDX-License-Identifier: AGPL-3.0-or-later
# Agricultural and landscape design formulas.
# Identical to the Python arm morie.fn.agridesign.

#' Agricultural design: buffer width, wetland sizing, range condition, rotations, variable-rate zones, drain spacing
#'
#' \code{BufferStripWidth}: first-order trapping \code{E(w) = 1 - exp(-k w)},
#' width \code{-ln(1 - target) / k}, with \code{k} estimated through the origin
#' from observed widths and efficiencies when not given.
#' \code{WetlandAreaPkc}: Kadlec-Knight P-k-C* sizing, plug flow
#' \code{A = (365 Q / k) ln((C_i - C*) / (C_o - C*))} (m^2, \code{Q} m^3/d,
#' \code{k} m/yr) or \code{n_tanks} tanks in series.
#' \code{RangeCondition}: Dyksterhuis (1949) similarity
#' \code{sum min(current, climax)} and its class. \code{RotationScore}: cyclic
#' pre-crop effect sum with ROTAT return-time, frequency and sequence screens
#' (crop codes are 0-based). \code{VariableRateZones}: yield-quantile zones and
#' Stanford mass-balance rates \code{max((n_per_yield Y - N_soil) / efficiency, 0)}.
#' \code{HooghoudtSpacing}: \code{q L^2 = 8 K_b d h + 4 K_a h^2} with Moody's
#' equivalent depth. \code{DrainWaterTable}: the mid-drain head for a given spacing.
#'
#' @param target Target removal efficiency (0 to 1).
#' @param k First-order width constant (1/m), or areal rate constant (m/yr).
#' @param widths,efficiencies Observed widths and removal efficiencies.
#' @param flow Inflow (m^3/d).
#' @param c_in,c_out,c_star Inlet, target outlet and background concentrations.
#' @param n_tanks Number of tanks in series, or NULL for plug flow.
#' @param current,climax Percent species composition now and in the climax community.
#' @param sequence Crop codes of one rotation cycle (0-based).
#' @param effect Matrix of pre-crop effects.
#' @param min_return Minimum return times per crop, or NULL.
#' @param max_frequency Maximum shares per crop, or NULL.
#' @param forbidden List of forbidden (preceding, following) pairs.
#' @param yield_map Cell yields.
#' @param n_zones Number of zones.
#' @param n_per_yield Nitrogen requirement per unit yield.
#' @param soil_n Soil nitrogen per cell, or NULL.
#' @param efficiency Fertiliser recovery efficiency.
#' @param q Drainage rate (m/d).
#' @param h Mid-drain head above drain level (m).
#' @param k_above,k_below Hydraulic conductivities above and below drain level (m/d).
#' @param depth_to_barrier Depth of the impermeable layer below drain level (m).
#' @param radius Drain radius (m).
#' @param tol,max_iter Fixed-point tolerance and iterations.
#' @param spacing Drain spacing (m).
#' @param equivalent_depth Equivalent depth (m).
#' @return A list, or a number for \code{DrainWaterTable}.
#' @references Zhang, X. et al. (2010). J. Environ. Qual. 39, 76-84.
#'   Kadlec, R. H. and Knight, R. L. (1996). Treatment Wetlands. CRC Press.
#'   Kadlec, R. H. and Wallace, S. D. (2009). Treatment Wetlands, 2nd ed.
#'   Dyksterhuis, E. J. (1949). J. Range Management 2, 104-115.
#'   Dogliotti, S. et al. (2003). Eur. J. Agron. 19, 239-250. Stanford, G.
#'   (1973). J. Environ. Qual. 2, 159-166. Ritzema, H. P. (1994). Drainage
#'   Principles and Applications, ILRI 16. Moody, W. T. (1966). J. Irrig.
#'   Drain. Div. ASCE 92, 1-9.
#' @examples
#' BufferStripWidth(0.9, k = 0.1)$width
#' WetlandAreaPkc(1000, 100, 20, 34, 5)$area_ha
#' HooghoudtSpacing(0.007, 0.8, 0.5, 0.5, 5)$spacing
#' @export
BufferStripWidth <- function(target, k = NULL, widths = NULL, efficiencies = NULL) {
  if (is.null(k)) {
    y <- -log(1 - as.numeric(efficiencies))
    w <- as.numeric(widths)
    k <- sum(w * y) / sum(w * w)
  }
  list(width = -log(1 - target) / k, k = k)
}

#' @rdname BufferStripWidth
#' @export
WetlandAreaPkc <- function(flow, c_in, c_out, k, c_star = 0, n_tanks = NULL) {
  ratio <- (c_out - c_star) / (c_in - c_star)
  area <- if (is.null(n_tanks)) 365 * flow / k * log(1 / ratio) else
    365 * flow / (k / (n_tanks * (ratio^(-1 / n_tanks) - 1)))
  list(area = area, area_ha = area / 1e4, hydraulic_loading = 365 * flow / area)
}

#' @rdname BufferStripWidth
#' @export
RangeCondition <- function(current, climax) {
  score <- sum(pmin(as.numeric(current), as.numeric(climax)))
  cls <- if (score > 75) "excellent" else if (score > 50) "good" else if (score > 25) "fair" else "poor"
  list(score = score, condition = cls)
}

#' @rdname BufferStripWidth
#' @export
RotationScore <- function(sequence, effect, min_return = NULL, max_frequency = NULL, forbidden = list()) {
  n <- length(sequence)
  nxt <- sequence[c(seq_len(n)[-1], 1)]
  score <- sum(vapply(seq_len(n), function(t) effect[sequence[t] + 1, nxt[t] + 1], 0))
  violations <- list()
  for (cr in sort(unique(sequence))) {
    pos <- which(sequence == cr) - 1
    gaps <- (pos[c(seq_along(pos)[-1], 1)] - pos) %% n
    gaps[gaps == 0] <- n
    if (!is.null(min_return) && min(gaps) < min_return[cr + 1]) violations <- c(violations, list(c("return", cr)))
    if (!is.null(max_frequency) && length(pos) / n > max_frequency[cr + 1]) {
      violations <- c(violations, list(c("frequency", cr)))
    }
  }
  for (t in seq_len(n)) for (f in forbidden) if (sequence[t] == f[1] && nxt[t] == f[2]) {
    violations <- c(violations, list(c("sequence", t - 1)))
  }
  list(score = score, feasible = length(violations) == 0, violations = violations)
}

.ag_q7 <- function(s, p) {
  h <- (length(s) - 1) * p
  lo <- floor(h)
  hi <- min(lo + 1, length(s) - 1)
  w <- h - lo
  if (w > 0) (1 - w) * s[lo + 1] + w * s[hi + 1] else s[lo + 1]
}

#' @rdname BufferStripWidth
#' @export
VariableRateZones <- function(yield_map, n_zones = 3, n_per_yield = 20, soil_n = NULL, efficiency = 1) {
  y <- as.numeric(yield_map)
  s <- sort(y)
  breaks <- vapply(seq_len(n_zones - 1), function(z) .ag_q7(s, z / n_zones), 0)
  zone <- vapply(y, function(v) sum(v > breaks), 0L)
  sn <- if (is.null(soil_n)) numeric(length(y)) else as.numeric(soil_n)
  means <- rates <- rep(NaN, n_zones)
  for (z in 0:(n_zones - 1)) {
    idx <- which(zone == z)
    if (length(idx)) {
      means[z + 1] <- sum(y[idx]) / length(idx)
      rates[z + 1] <- max((n_per_yield * means[z + 1] - sum(sn[idx]) / length(idx)) / efficiency, 0)
    }
  }
  list(zone = zone, breaks = breaks, zone_mean = means, rate = rates)
}

.ag_eqdepth <- function(D, L, r) {
  x <- D / L
  if (x <= 0.3) D / (1 + x * (8 / pi * log(D / r) - 3.4)) else pi * L / (8 * (log(L / r) - 1.15))
}

#' @rdname BufferStripWidth
#' @export
HooghoudtSpacing <- function(q, h, k_above, k_below, depth_to_barrier, radius = 0.1, tol = 1e-10, max_iter = 200) {
  d <- depth_to_barrier
  L <- sqrt((8 * k_below * d * h + 4 * k_above * h * h) / q)
  for (it in seq_len(max_iter)) {
    d <- .ag_eqdepth(depth_to_barrier, L, radius)
    nw <- sqrt((8 * k_below * d * h + 4 * k_above * h * h) / q)
    if (abs(nw - L) <= tol * nw) {
      L <- nw
      break
    }
    L <- nw
  }
  list(spacing = L, equivalent_depth = .ag_eqdepth(depth_to_barrier, L, radius))
}

#' @rdname BufferStripWidth
#' @export
DrainWaterTable <- function(q, spacing, k_above, k_below, equivalent_depth) {
  a <- 4 * k_above
  b <- 8 * k_below * equivalent_depth
  cc <- -q * spacing * spacing
  if (a == 0) return(-cc / b)
  (-b + sqrt(b * b - 4 * a * cc)) / (2 * a)
}
