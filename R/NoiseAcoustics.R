.nz_db <- function(e) 10 * log10(e)

#' Environmental noise level statistics and rating indicators
#'
#' \code{EquivalentLevel}: Leq = 10 log10(sum t_i 10^(L_i/10) / sum t_i). \code{LevelStatistics}: \code{L_N} levels (the \code{1 - N/100}
#' type-7 quantile of equally spaced samples), Leq, Lmax, Lmin.
#' \code{SoundExposureLevel}: SEL of samples \code{dt} seconds apart
#' (\code{T0 = 1} s). \code{CombineLevels}: energetic sum.
#' \code{DayEveningNight}: Lday, Levening, Lnight and Lden (Directive
#' 2002/49/EC Annex I) from 24 hourly levels, with US Ldn (night 22-07, +10
#' dB) and California CNEL (evening 19-22 weighted by 3, night weighted by 10).
#' \code{AircraftDnl}: DNL from single-event SELs, normalised by 86400 s.
#' Identical to the Python arm \code{morie.fn.noiseacoustics}.
#'
#' @param levels Sound levels (dB).
#' @param durations Durations (default equal).
#' @param percentiles Exceedance percentages.
#' @param dt Sample spacing (s).
#' @param hourly 24 hourly Leq values, index = starting hour.
#' @param day,evening,night Start and end hours of the Lden windows.
#' @param evening_penalty,night_penalty Lden penalties (dB).
#' @param sel_day,sel_night Event SELs by period.
#' @return Numeric or list.
#' @references Directive 2002/49/EC relating to the assessment and management
#'   of environmental noise, Annex I.
#'
#'   US EPA (1974). Information on Levels of Environmental Noise Requisite to
#'   Protect Public Health and Welfare with an Adequate Margin of Safety.
#'   EPA 550/9-74-004.
#' @examples
#' EquivalentLevel(c(60, 70))
#' LevelStatistics(c(50, 52, 55, 60, 71))$L10
#' DayEveningNight(rep(60, 24))$Lden
#' @export
EquivalentLevel <- function(levels, durations = NULL) {
  if (is.null(durations)) durations <- rep(1, length(levels))
  .nz_db(sum(durations * 10^(levels / 10)) / sum(durations))
}

#' @rdname EquivalentLevel
#' @export
LevelStatistics <- function(levels, percentiles = c(10, 50, 90)) {
  out <- lapply(percentiles, function(p) unname(stats::quantile(levels, 1 - p / 100, type = 7)))
  names(out) <- paste0("L", format(percentiles, trim = TRUE))
  c(out, list(Leq = EquivalentLevel(levels), Lmax = max(levels), Lmin = min(levels)))
}

#' @rdname EquivalentLevel
#' @export
SoundExposureLevel <- function(levels, dt = 1) .nz_db(sum(10^(levels / 10)) * dt)

#' @rdname EquivalentLevel
#' @export
CombineLevels <- function(levels) .nz_db(sum(10^(levels / 10)))

.nz_hours <- function(a, b) if (a < b) seq(a, b - 1) else c(seq(a, 23), seq_len(b) - 1)

#' @rdname EquivalentLevel
#' @export
DayEveningNight <- function(hourly, day = c(7, 19), evening = c(19, 23), night = c(23, 7), evening_penalty = 5,
                            night_penalty = 10) {
  if (length(hourly) != 24) stop("hourly must hold 24 values")
  E <- 10^(hourly / 10)
  es <- function(hs) sum(E[hs + 1])
  hd <- .nz_hours(day[1], day[2])
  he <- .nz_hours(evening[1], evening[2])
  hn <- .nz_hours(night[1], night[2])
  list(Lday = .nz_db(es(hd) / length(hd)), Levening = .nz_db(es(he) / length(he)), Lnight = .nz_db(es(hn) / length(hn)),
       Lden = .nz_db((es(hd) + 10^(evening_penalty / 10) * es(he) + 10^(night_penalty / 10) * es(hn)) / 24),
       Ldn = .nz_db((es(.nz_hours(7, 22)) + 10 * es(.nz_hours(22, 7))) / 24),
       CNEL = .nz_db((es(.nz_hours(7, 19)) + 3 * es(.nz_hours(19, 22)) + 10 * es(.nz_hours(22, 7))) / 24))
}

#' @rdname EquivalentLevel
#' @export
AircraftDnl <- function(sel_day, sel_night = numeric(0)) {
  .nz_db(sum(10^(sel_day / 10)) + 10 * sum(10^(sel_night / 10))) - .nz_db(86400)
}

#' Outdoor sound propagation (ISO 9613)
#'
#' \code{AtmosphericAbsorption}: ISO 9613-1 pure-tone absorption coefficient
#' (dB/m). \code{PointSourceLevel}: \eqn{L_p = L_W + D_c - (20\log_{10} d +
#' 11) - \alpha d - A_{extra}}. \code{AttenuationDistance}: distance at which
#' that level reaches a threshold. \code{GroundAttenuation}: ISO 9613-2
#' alternative ground attenuation and \eqn{D_\Omega}.
#' \code{BarrierAttenuation}: ISO 9613-2 screening \eqn{D_z} (single or double
#' diffraction). \code{FoliageAttenuation}: ISO 9613-2 Table A.1.
#' \code{NoiseMap}: energetic sum over point sources. \code{NoiseZones}:
#' reporting bands and exposed counts.
#'
#' @param f Frequencies (Hz).
#' @param temperature_c,humidity,pressure_kpa Atmospheric state.
#' @param lw Sound power level(s).
#' @param d Distance(s) (m).
#' @param alpha Absorption (dB/m).
#' @param dc Directivity correction (dB).
#' @param extra Further attenuation (dB).
#' @param threshold Target level.
#' @param dp,hs,hr Horizontal distance, source and receiver heights.
#' @param d_ss,d_sr Source-to-edge and edge-to-receiver distances.
#' @param a Lateral offset parallel to the barrier edge.
#' @param e Distance between the two edges (double diffraction).
#' @param c Speed of sound.
#' @param df Path length through foliage.
#' @param band Octave midband frequency (63 to 8000).
#' @param sources,receivers Two-column coordinate matrices.
#' @param min_distance Minimum distance.
#' @param breaks Band limits.
#' @param population Optional counts.
#' @return Numeric or list.
#' @references ISO 9613-1:1993 and ISO 9613-2:1996, Acoustics -- Attenuation
#'   of sound during propagation outdoors, Parts 1 and 2.
#' @examples
#' 1000 * AtmosphericAbsorption(1000)
#' PointSourceLevel(100, c(10, 100))
#' BarrierAttenuation(10, 10, 19, 1000)$Dz
#' @export
AtmosphericAbsorption <- function(f, temperature_c = 20, humidity = 70, pressure_kpa = 101.325) {
  tk <- temperature_c + 273.15
  T0 <- 293.15
  pa <- pressure_kpa / 101.325
  C <- -6.8346 * (273.16 / tk)^1.261 + 4.6151
  h <- humidity * 10^C / pa
  frO <- pa * (24 + 4.04e4 * h * (0.02 + h) / (0.391 + h))
  frN <- pa * (tk / T0)^-0.5 * (9 + 280 * h * exp(-4.170 * ((tk / T0)^(-1 / 3) - 1)))
  f2 <- f * f
  8.686 * f2 * (1.84e-11 / pa * (tk / T0)^0.5 + (tk / T0)^-2.5 *
                  (0.01275 * exp(-2239.1 / tk) / (frO + f2 / frO) + 0.1068 * exp(-3352.0 / tk) / (frN + f2 / frN)))
}

#' @rdname AtmosphericAbsorption
#' @export
PointSourceLevel <- function(lw, d, alpha = 0, dc = 0, extra = 0) lw + dc - (20 * log10(d) + 11) - alpha * d - extra

#' @rdname AtmosphericAbsorption
#' @export
AttenuationDistance <- function(lw, threshold, alpha = 0, dc = 0) {
  d <- 10^((lw + dc - 11 - threshold) / 20)
  if (alpha > 0) {
    for (it in 1:100) {
      g <- lw + dc - 20 * log10(d) - 11 - alpha * d - threshold
      step <- g / (20 / (d * log(10)) + alpha)
      d <- max(d + step, d / 10)
      if (abs(step) < 1e-12 * d) break
    }
  }
  d
}

#' @rdname AtmosphericAbsorption
#' @export
GroundAttenuation <- function(dp, hs, hr) {
  d <- sqrt(dp^2 + (hs - hr)^2)
  hm <- (hs + hr) / 2
  list(A_gr = max(0, 4.8 - (2 * hm / d) * (17 + 300 / d)),
       D_omega = 10 * log10(1 + (dp^2 + (hs - hr)^2) / (dp^2 + (hs + hr)^2)))
}

#' @rdname AtmosphericAbsorption
#' @export
BarrierAttenuation <- function(d_ss, d_sr, d, f, a = 0, e = 0, c = 340) {
  lam <- c / f
  if (e > 0) {
    z <- sqrt((d_ss + d_sr + e)^2 + a^2) - d
    r5 <- (5 * lam / e)^2
    c3 <- (1 + r5) / (1 / 3 + r5)
    cap <- 25
  } else {
    z <- sqrt((d_ss + d_sr)^2 + a^2) - d
    c3 <- 1
    cap <- 20
  }
  if (z <= 0) return(list(Dz = 0, z = z, K_met = 1))
  km <- exp(-sqrt(d_ss * d_sr * d / (2 * z)) / 2000)
  list(Dz = min(cap, 10 * log10(3 + 20 / lam * c3 * z * km)), z = z, K_met = km)
}

#' @rdname AtmosphericAbsorption
#' @export
FoliageAttenuation <- function(df, band) {
  tab <- rbind(c(63, 0, 0.02), c(125, 0, 0.03), c(250, 1, 0.04), c(500, 1, 0.05), c(1000, 1, 0.06),
               c(2000, 1, 0.08), c(4000, 2, 0.09), c(8000, 3, 0.12))
  k <- match(band, tab[, 1])
  if (is.na(k)) stop("band must be an octave midband frequency 63..8000 Hz")
  if (df < 10) return(0)
  if (df <= 20) return(tab[k, 2])
  tab[k, 3] * min(df, 200)
}

#' @rdname AtmosphericAbsorption
#' @export
NoiseMap <- function(lw, sources, receivers, alpha = 0, dc = 0, min_distance = 1) {
  S <- as.matrix(sources)
  R <- as.matrix(receivers)
  vapply(seq_len(nrow(R)), function(i) {
    d <- pmax(sqrt((S[, 1] - R[i, 1])^2 + (S[, 2] - R[i, 2])^2), min_distance)
    .nz_db(sum(10^(PointSourceLevel(lw, d, alpha, dc) / 10)))
  }, 0)
}

#' @rdname AtmosphericAbsorption
#' @export
NoiseZones <- function(levels, breaks = c(55, 60, 65, 70, 75), population = NULL) {
  B <- sort(breaks)
  z <- vapply(levels, function(v) sum(v >= B), 0L)
  w <- if (is.null(population)) rep(1, length(z)) else population
  counts <- vapply(0:length(B), function(k) sum(w[z == k]), 0)
  list(zone = z, counts = counts, breaks = B)
}

#' Road, construction, vibration and underwater noise; dose-response
#'
#' \code{CrtnRoadNoise}: UK CRTN (1988) \eqn{L_{A10}} with speed/heavy,
#' gradient, distance, ground, angle-of-view and facade corrections.
#' \code{ConstructionNoise}: FHWA RCNM Lmax and Leq. \code{VibrationPropagation}:
#' FTA velocity-level (\code{-30 log10(D/Dref)}) and PPV (\code{(Dref/D)^1.5})
#' attenuation. \code{UnderwaterPropagation}: Thorp absorption with
#' spherical/cylindrical spreading. \code{NoiseAnnoyance}: Miedema and
#' Oudshoorn (2001) percentage highly annoyed. \code{NoiseHealthRisk}:
#' log-linear relative risk above a threshold (WHO 2018 road traffic and
#' ischaemic heart disease by default) and the attributable fraction.
#'
#' @param flow Hourly (or 18-hour) traffic flow.
#' @param speed Mean speed (km/h).
#' @param heavy_pct Percentage of heavy vehicles.
#' @param gradient_pct Road gradient (percent).
#' @param distance Distance(s).
#' @param receiver_height Receiver height (m).
#' @param soft_ground Absorbent ground fraction.
#' @param angle Angle of view (degrees).
#' @param facade Add the 2.5 dB facade correction.
#' @param period \code{"hour"} or \code{"18h"}.
#' @param lmax50 Lmax at 50 ft.
#' @param usage_factor Usage factor (percent).
#' @param ref_level Level at the reference distance.
#' @param ref_distance Reference distance (25 ft).
#' @param kind \code{"lv"} or \code{"ppv"}.
#' @param source_level Source level (dB re 1 uPa at 1 m).
#' @param r Range(s) (m).
#' @param f_khz Frequency (kHz).
#' @param transition_range Spherical-to-cylindrical transition range.
#' @param level Exposure level(s).
#' @param source \code{"aircraft"}, \code{"road"} or \code{"rail"}.
#' @param rr_per_10db Relative risk per 10 dB.
#' @param threshold Exposure threshold.
#' @param population Optional population counts per level.
#' @return Numeric or list.
#' @references Department of Transport and Welsh Office (1988). Calculation of
#'   Road Traffic Noise. HMSO.
#'
#'   Miedema, H. M. E. and Oudshoorn, C. G. M. (2001). Annoyance from
#'   transportation noise. Environmental Health Perspectives 109, 409-416.
#'
#'   Thorp, W. H. (1967). Analytic description of the low-frequency
#'   attenuation coefficient. Journal of the Acoustical Society of America 42,
#'   270.
#' @examples
#' CrtnRoadNoise(1000)$L10
#' NoiseAnnoyance(60, "road")
#' UnderwaterPropagation(200, 1000, 10)$RL
#' @export
CrtnRoadNoise <- function(flow, speed = 75, heavy_pct = 0, gradient_pct = 0, distance = 10, receiver_height = 1.5,
                          soft_ground = 0, angle = 180, facade = FALSE, period = "hour") {
  basic <- (if (period == "hour") 42.2 else 29.1) + 10 * log10(flow)
  c_speed <- 33 * log10(speed + 40 + 500 / speed) + 10 * log10(1 + 5 * heavy_pct / speed) - 68.8
  c_dist <- -10 * log10(sqrt((distance + 3.5)^2 + (receiver_height - 0.5)^2) / 13.5)
  H <- (receiver_height + 0.5) / 2
  c_ground <- if (soft_ground <= 0 || H >= (distance + 5) / 6) {
    0
  } else if (H < 0.75) {
    5.2 * soft_ground * log10(3 / (distance + 3.5))
  } else {
    5.2 * soft_ground * log10((6 * H - 1.5) / (distance + 3.5))
  }
  parts <- c(basic = basic, speed = c_speed, gradient = 0.3 * gradient_pct, distance = c_dist, ground = c_ground,
             angle = 10 * log10(angle / 180), facade = if (facade) 2.5 else 0)
  list(L10 = sum(parts), corrections = parts)
}

#' @rdname CrtnRoadNoise
#' @export
ConstructionNoise <- function(lmax50, distance, usage_factor = 100) {
  lmax <- lmax50 - 20 * log10(distance / 50)
  list(Lmax = lmax, Leq = lmax + 10 * log10(usage_factor / 100))
}

#' @rdname CrtnRoadNoise
#' @export
VibrationPropagation <- function(ref_level, distance, ref_distance = 25, kind = "lv") {
  if (kind == "lv") return(ref_level - 30 * log10(distance / ref_distance))
  if (kind == "ppv") return(ref_level * (ref_distance / distance)^1.5)
  stop("kind must be lv or ppv")
}

#' @rdname CrtnRoadNoise
#' @export
UnderwaterPropagation <- function(source_level, r, f_khz, transition_range = NULL) {
  f2 <- f_khz^2
  alpha <- 0.11 * f2 / (1 + f2) + 44 * f2 / (4100 + f2) + 2.75e-4 * f2 + 0.003
  s <- if (is.null(transition_range)) {
    20 * log10(r)
  } else {
    ifelse(r <= transition_range, 20 * log10(r), 20 * log10(transition_range) + 10 * log10(r / transition_range))
  }
  tl <- s + alpha * r / 1000
  list(alpha = alpha, TL = tl, RL = source_level - tl)
}

#' @rdname CrtnRoadNoise
#' @export
NoiseAnnoyance <- function(level, source = "road") {
  cf <- switch(source, aircraft = c(-9.199e-5, 3.932e-2, 0.2939), road = c(9.868e-4, -1.436e-2, 0.5118),
               rail = c(7.239e-4, -7.851e-3, 0.1695), stop("source must be aircraft, road or rail"))
  x <- level - 42
  ifelse(level <= 42, 0, cf[1] * x^3 + cf[2] * x^2 + cf[3] * x)
}

#' @rdname CrtnRoadNoise
#' @export
NoiseHealthRisk <- function(level, rr_per_10db = 1.08, threshold = 53, population = NULL) {
  rr <- ifelse(level > threshold, rr_per_10db^((level - threshold) / 10), 1)
  out <- list(rr = rr)
  if (!is.null(population)) {
    x <- sum(population / sum(population) * (rr - 1))
    out$paf <- x / (1 + x)
  }
  out
}
