# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of hadcrut -- HadCRUT5 blended near-surface temperature anomaly
# with uncertainty. Mirrors src/morie/fn/hadcrut.py operation for
# operation, on the shared numerics in R/aaa_helpers_w3num.R.
#
# A global temperature record is two measurement systems stapled
# together. Over land it is air temperature from weather stations; over
# the ocean it is water temperature from ships and buoys. They are
# different physical quantities measured by different instruments, and
# the record is the weighted average of the two in each 5 degree cell.
#
# The weights are where the judgement lives, and HadCRUT5 makes three
# decisions worth spelling out, because each of them changes the answer:
#
#   * the weight is the AREAL FRACTION of land and sea in the cell, from
#     the OSTIA land mask;
#   * land air temperature is given a MINIMUM weighting of 25%, so that
#     a single island station in an otherwise oceanic cell is not
#     averaged into nothing. The floor applies only where the land data
#     set actually reports the cell -- in a cell with no land
#     observation the ocean takes the whole weight, and no floor is
#     invented for a station that is not there;
#   * water under sea ice is not ocean for this purpose. Ice
#     concentration above 15% counts the area as ice covered, and
#     ice-covered water is treated as LAND when the weights are derived.
#
# Where only one of the two data sets reports a cell, that one gets the
# whole weight.
#
# Averaging up from cells is the second place a choice hides. A cell at
# 70 degrees north covers a fraction of the area of a cell on the
# equator, so cells enter an area mean weighted by the cosine of their
# latitude. HadCRUT5's headline global series is not that: it is the
# UNWEIGHTED mean of the two hemispheric means, which is a deliberate
# coverage decision -- it stops the better-observed hemisphere from
# carrying the global figure. Both are here, and so is the land-record
# convention of weighting the northern hemisphere twice, because the
# routes disagree exactly when coverage is asymmetric, which is most of
# the nineteenth century.
#
# Uncertainty is carried as three separate things, because they do not
# combine the same way: uncorrelated measurement and sampling error,
# which shrinks under averaging; correlated bias adjustment error, which
# does not, and is carried by an ensemble; and coverage error, which
# cannot be computed from the observations at all and is estimated by
# masking a COMPLETE reference field to the coverage actually achieved.
# With no reference field that term is reported as NULL, not guessed.
#
# References
#   Morice, C.P., Kennedy, J.J., Rayner, N.A., Winn, J.P., Hogan, E.,
#     Killick, R.E., Dunn, R.J.H., Osborn, T.J., Jones, P.D. and
#     Simpson, I.R. (2021) "An updated assessment of near-surface
#     temperature change from 1850: the HadCRUT5 data set." Journal of
#     Geophysical Research: Atmospheres 126(3), e2019JD032361.
#     doi:10.1029/2019JD032361.
#   Osborn, T.J. et al. (2021) "Land surface air temperature variations
#     across the globe updated to 2019: the CRUTEM5 data set." Journal
#     of Geophysical Research: Atmospheres 126(2), e2019JD032352.
#   Kennedy, J.J., Rayner, N.A., Atkinson, C.P. and Killick, R.E. (2019)
#     "An ensemble data set of sea surface temperature change from 1850:
#     the Met Office Hadley Centre HadSST.4.0.0.0 data set." Journal of
#     Geophysical Research: Atmospheres 124(14), 7719-7763.
#   Donlon, C.J. et al. (2012) "The Operational Sea Surface Temperature
#     and Sea Ice Analysis (OSTIA) system." Remote Sensing of
#     Environment 116, 140-158.

.HADCRUT_WEIGHT_RULES <- c("hadcrut5", "area", "land_only", "sst_only")
.HADCRUT_MEAN_ROUTES <- c("hemispheric", "area", "land_ratio")
.HADCRUT_INTERVALS <- c("normal", "ensemble")

# The minimum weight land air temperature receives in a cell the land
# data set reports, so an island station is not averaged away.
.HADCRUT_LAND_FLOOR <- 0.25
# Ice concentration at or above this counts the area as ice covered, and
# ice-covered water is weighted as land.
.HADCRUT_ICE_THRESHOLD <- 0.15

#' Land and ocean blending weights for one grid cell
#'
#' The floor is applied only when the land data set reports the cell.
#' Applying it to an unobserved cell would put weight on a station that
#' does not exist, which is the one thing the rule is not for.
#'
#' @param land_fraction Areal land fraction of the cell.
#' @param sea_ice Sea-ice concentration of the cell.
#' @param has_land Whether the land data set reports the cell.
#' @param has_sst Whether the SST data set reports the cell.
#' @param rule A member of the weight-rule list.
#' @return A numeric pair, the land and ocean weights, summing to one
#'   when the cell has any observation and to zero when it has none.
#' @export
morie_hadcrut_weights <- function(land_fraction, sea_ice = 0,
                                  has_land = TRUE, has_sst = TRUE,
                                  rule = "hadcrut5") {
  if (!(rule %in% .HADCRUT_WEIGHT_RULES))
    stop("rule must be one of ", paste(.HADCRUT_WEIGHT_RULES, collapse = ", "))
  lf <- as.numeric(land_fraction)
  if (lf < 0 || lf > 1) stop("land_fraction must lie in [0, 1]")
  ice <- as.numeric(sea_ice)
  if (ice < 0 || ice > 1) stop("sea_ice must lie in [0, 1]")
  if (rule == "land_only") return(if (has_land) c(1, 0) else c(0, 0))
  if (rule == "sst_only") return(if (has_sst) c(0, 1) else c(0, 0))
  # Ice-covered water is land for the purpose of the weights. Below the
  # concentration threshold the ice is ignored entirely rather than
  # scaled down -- the rule is a threshold, not a ramp.
  frac_ice <- if (ice >= .HADCRUT_ICE_THRESHOLD) ice else 0
  eff <- lf + (1 - lf) * frac_ice
  if (!has_land && !has_sst) return(c(0, 0))
  if (!has_sst) return(c(1, 0))
  if (!has_land) return(c(0, 1))
  if (rule == "hadcrut5" && eff < .HADCRUT_LAND_FLOOR)
    eff <- .HADCRUT_LAND_FLOOR
  c(eff, 1 - eff)
}

# Centre latitude of row i (one-based) of an n_lat band grid, south to
# north.
#' Centre latitude of row i (one-based) of an n_lat band grid, south to
#'
#' north.
#'
#' @param i See Usage.
#' @param n_lat See Usage.
#' @return A numeric value.
#' @export
.hadcrut_cell_lat <- function(i, n_lat) -90 + (i - 0.5) * (180 / n_lat)

# Area weight of a latitude band: the cosine of its centre.
#' Area weight of a latitude band: the cosine of its centre
#'
#' Part of the hadcrut_native implementation; see the file header for
#' the source it follows.
#'
#' @param i See Usage.
#' @param n_lat See Usage.
#' @return The value of \code{cos}.
#' @export
.hadcrut_band_weight <- function(i, n_lat)
  cos(.hadcrut_cell_lat(i, n_lat) * pi / 180)

#' Blend a land grid and an SST grid cell by cell
#'
#' @param T Land air temperature anomaly matrix, NA where unobserved.
#' @param sst Sea-surface temperature anomaly matrix, likewise.
#' @param land_fraction Areal land fraction matrix.
#' @param sea_ice Sea-ice concentration matrix, or NULL.
#' @param rule A member of the weight-rule list.
#' @param T_var Per-cell land error variance matrix, or NULL.
#' @param sst_var Per-cell SST error variance matrix, or NULL.
#' @return A list with the blended anomaly matrix, the blended variance
#'   matrix, the land weights and the observation mask.
#' @export
morie_hadcrut_blend <- function(T, sst, land_fraction, sea_ice = NULL,
                                rule = "hadcrut5", T_var = NULL,
                                sst_var = NULL) {
  n_lat <- nrow(T); n_lon <- ncol(T)
  anom <- matrix(NA_real_, n_lat, n_lon)
  var <- matrix(NA_real_, n_lat, n_lon)
  wl <- matrix(0, n_lat, n_lon)
  seen <- matrix(FALSE, n_lat, n_lon)
  for (i in seq_len(n_lat)) for (j in seq_len(n_lon)) {
    tl <- T[i, j]; ts <- sst[i, j]
    hl <- !is.na(tl); hs <- !is.na(ts)
    ice <- if (is.null(sea_ice)) 0 else as.numeric(sea_ice[i, j])
    ab <- morie_hadcrut_weights(land_fraction[i, j], ice, hl, hs, rule)
    a <- ab[1]; b <- ab[2]
    if (a + b <= 0) next
    v <- 0
    if (a > 0) v <- v + a * as.numeric(tl)
    if (b > 0) v <- v + b * as.numeric(ts)
    anom[i, j] <- v
    if (!(is.null(T_var) && is.null(sst_var))) {
      # Independent sources, so the variances add through the SQUARED
      # weights -- the usual trap is to add them through the weights
      # themselves, which understates a near-even blend and overstates
      # a lopsided one.
      q <- 0
      if (a > 0 && !is.null(T_var) && !is.na(T_var[i, j]))
        q <- q + a * a * as.numeric(T_var[i, j])
      if (b > 0 && !is.null(sst_var) && !is.na(sst_var[i, j]))
        q <- q + b * b * as.numeric(sst_var[i, j])
      var[i, j] <- q
    }
    wl[i, j] <- a
    seen[i, j] <- TRUE
  }
  list(anomaly = anom, variance = var, land_weight = wl, observed = seen)
}

# Cosine-weighted mean over the given rows, and its variance. The mean
# is NA when the region holds no observation, which is a real state in
# 1850 and must not silently become zero.
#' Cosine-weighted mean over the given rows, and its variance. The mean
#'
#' is NA when the region holds no observation, which is a real state in
#' 1850 and must not silently become zero.
#'
#' @param grid See Usage.
#' @param rows See Usage.
#' @param var Defaults to \code{NULL}.
#' @return A list with \code{mean}, \code{var}, \code{weight}, \code{n}.
#' @export
.hadcrut_region_mean <- function(grid, rows, var = NULL) {
  n_lat <- nrow(grid)
  num <- numeric(0); den <- numeric(0); qnum <- numeric(0); n <- 0L
  for (i in rows) {
    w <- .hadcrut_band_weight(i, n_lat)
    for (j in seq_len(ncol(grid))) {
      if (is.na(grid[i, j])) next
      num <- c(num, w * grid[i, j])
      den <- c(den, w)
      n <- n + 1L
      if (!is.null(var) && !is.na(var[i, j]))
        qnum <- c(qnum, w * w * var[i, j])
    }
  }
  if (!length(den)) return(list(mean = NA_real_, var = NA_real_,
                                weight = 0, n = 0L))
  d <- .w3_csum(den)
  list(mean = .w3_csum(num) / d,
       var = if (length(qnum)) .w3_csum(qnum) / (d * d) else NA_real_,
       weight = d, n = n)
}

#' Average a grid up to a global figure
#'
#' "area" is one cosine-weighted mean over every observed cell;
#' "hemispheric" is the mean of the two hemispheric means, which is
#' HadCRUT5's headline convention and stops the better-observed
#' hemisphere from carrying the global number; "land_ratio" is the
#' land-record convention, two parts north to one part south, in the
#' ratio of the hemispheres' land areas.
#'
#' @param grid Anomaly matrix, NA where unobserved.
#' @param route A member of the mean-route list.
#' @param var Per-cell variance matrix, or NULL.
#' @return A list with the mean, its variance, the total weight, the
#'   cell counts and the two hemispheric means.
#' @export
morie_hadcrut_area_mean <- function(grid, route = "hemispheric", var = NULL) {
  if (!(route %in% .HADCRUT_MEAN_ROUTES))
    stop("route must be one of ", paste(.HADCRUT_MEAN_ROUTES, collapse = ", "))
  n_lat <- nrow(grid)
  lats <- vapply(seq_len(n_lat), function(i) .hadcrut_cell_lat(i, n_lat),
                 numeric(1))
  south <- which(lats < 0); north <- which(lats >= 0)
  s <- .hadcrut_region_mean(grid, south, var)
  nn <- .hadcrut_region_mean(grid, north, var)
  if (route == "area") {
    g <- .hadcrut_region_mean(grid, seq_len(n_lat), var)
    out <- list(mean = g$mean, var = g$var, weight = g$weight,
                n_cells = g$n)
  } else {
    ab <- if (route == "hemispheric") c(0.5, 0.5) else c(2 / 3, 1 / 3)
    a <- ab[1]; b <- ab[2]
    if (is.na(nn$mean) && is.na(s$mean)) {
      out <- list(mean = NA_real_, var = NA_real_, weight = 0, n_cells = 0L)
    } else if (is.na(nn$mean)) {
      out <- list(mean = s$mean, var = s$var, weight = s$weight,
                  n_cells = s$n)
    } else if (is.na(s$mean)) {
      out <- list(mean = nn$mean, var = nn$var, weight = nn$weight,
                  n_cells = nn$n)
    } else {
      out <- list(mean = a * nn$mean + b * s$mean,
                  var = if (is.na(nn$var) || is.na(s$var)) NA_real_
                        else a * a * nn$var + b * b * s$var,
                  weight = nn$weight + s$weight, n_cells = nn$n + s$n)
    }
  }
  out$north <- nn$mean; out$south <- s$mean
  out$n_north <- nn$n; out$n_south <- s$n
  out$route <- route
  out
}

#' The coverage error of one complete field under one coverage mask
#'
#' The complete field is averaged twice: over everything, and over only
#' the cells the observations actually reach. The difference is one
#' realisation of the coverage error -- not a bound on it, a draw from
#' it -- and the root mean square over several reference fields is the
#' coverage uncertainty. This is the only honest way to get the term: it
#' cannot be derived from the observations, because the observations are
#' precisely what is missing where it matters.
#'
#' @param reference A complete anomaly matrix.
#' @param seen The observation mask.
#' @param route A member of the mean-route list.
#' @return The signed coverage error, or NA when either average is
#'   undefined.
#' @export
morie_hadcrut_coverage_error <- function(reference, seen,
                                         route = "hemispheric") {
  full <- morie_hadcrut_area_mean(reference, route)$mean
  masked <- reference
  masked[!seen] <- NA_real_
  part <- morie_hadcrut_area_mean(masked, route)$mean
  if (is.na(full) || is.na(part)) return(NA_real_)
  part - full
}

#' Blend land and ocean anomaly grids and average them up
#'
#' @param T Land air temperature anomaly matrix on a regular grid
#'   running south to north, NA where unobserved.
#' @param sst Sea-surface temperature anomaly matrix on the same grid.
#' @param land_fraction Areal land fraction per cell; all ocean when
#'   omitted.
#' @param sea_ice Sea-ice concentration per cell, or NULL. Concentration
#'   at or above the threshold makes the water count as land.
#' @param rule A member of the weight-rule list.
#' @param route A member of the mean-route list.
#' @param interval "normal" builds the interval from the combined
#'   standard error; "ensemble" takes it from the spread of the
#'   ensemble, which is the only route that carries the correlated bias
#'   term properly.
#' @param level Interval coverage.
#' @param T_var Per-cell land error variance matrix, or NULL.
#' @param sst_var Per-cell SST error variance matrix, or NULL.
#' @param ensemble A list of realisations of the blended field, or NULL.
#' @param reference A list of complete fields for the coverage term, or
#'   NULL.
#' @return A list with the blended grid, the global and hemispheric
#'   means, the three uncertainty components and the combined interval,
#'   and the coverage actually achieved.
#' @export
morie_hadcrut <- function(T, sst, land_fraction = NULL, sea_ice = NULL,
                          rule = "hadcrut5", route = "hemispheric",
                          interval = "normal", level = 0.95, T_var = NULL,
                          sst_var = NULL, ensemble = NULL,
                          reference = NULL) {
  if (!(rule %in% .HADCRUT_WEIGHT_RULES))
    stop("rule must be one of ", paste(.HADCRUT_WEIGHT_RULES, collapse = ", "))
  if (!(route %in% .HADCRUT_MEAN_ROUTES))
    stop("route must be one of ", paste(.HADCRUT_MEAN_ROUTES, collapse = ", "))
  if (!(interval %in% .HADCRUT_INTERVALS))
    stop("interval must be one of ", paste(.HADCRUT_INTERVALS, collapse = ", "))
  level <- as.numeric(level)
  if (!(level > 0 && level < 1))
    stop("level must lie strictly inside (0, 1)")
  T <- as.matrix(T); sst <- as.matrix(sst)
  storage.mode(T) <- "double"; storage.mode(sst) <- "double"
  n_lat <- nrow(T); n_lon <- ncol(T)
  if (n_lat < 2L) stop("need at least two latitude bands")
  if (nrow(sst) != n_lat || ncol(sst) != n_lon)
    stop("sst must be a rectangular grid matching T")
  if (is.null(land_fraction))
    land_fraction <- matrix(0, n_lat, n_lon)

  bl <- morie_hadcrut_blend(T, sst, land_fraction, sea_ice, rule, T_var,
                            sst_var)
  anom <- bl$anomaly; seen <- bl$observed
  vg <- if (is.null(T_var) && is.null(sst_var)) NULL else bl$variance
  agg <- morie_hadcrut_area_mean(anom, route, vg)
  est <- agg$mean

  n_obs <- sum(seen)
  # Coverage as a fraction of AREA, not of cells: a missing polar cell
  # is much less of a gap than a missing tropical one.
  bw <- vapply(seq_len(n_lat), function(i) .hadcrut_band_weight(i, n_lat),
               numeric(1))
  tot <- .w3_csum(rep(bw, each = n_lon))
  got <- .w3_csum(rep(bw, each = n_lon)[as.vector(t(seen))])
  se_unc <- if (!is.na(agg$var)) sqrt(agg$var) else NULL

  se_cor <- NULL; members <- NULL
  if (!is.null(ensemble)) {
    members <- vapply(ensemble, function(g)
      morie_hadcrut_area_mean(g, route)$mean, numeric(1))
    members <- members[!is.na(members)]
    if (length(members) > 1L) {
      mm <- .w3_csum(members) / length(members)
      se_cor <- sqrt(.w3_csum((members - mm) * (members - mm)) /
                       (length(members) - 1))
    }
  }

  se_cov <- NULL; cov_draws <- NULL
  if (!is.null(reference)) {
    cov_draws <- vapply(reference, function(r)
      morie_hadcrut_coverage_error(r, seen, route), numeric(1))
    cov_draws <- cov_draws[!is.na(cov_draws)]
    if (length(cov_draws))
      se_cov <- sqrt(.w3_csum(cov_draws * cov_draws) / length(cov_draws))
  }

  parts <- c(se_unc, se_cor, se_cov)
  se <- if (length(parts)) sqrt(.w3_csum(parts * parts)) else NULL

  lo <- NULL; hi <- NULL
  if (!is.na(est)) {
    if (interval == "ensemble" && !is.null(members) && length(members) > 1L) {
      srt <- sort(members, method = "radix")
      # The empirical quantile at the nearest rank, which is the
      # convention that needs no interpolation and therefore cannot
      # disagree between two languages' quantile types.
      for (tail in c(0.5 * (1 - level), 1 - 0.5 * (1 - level))) {
        k <- ceiling(tail * length(srt))
        if (k < 1) k <- 1L
        if (k > length(srt)) k <- length(srt)
        if (tail < 0.5) lo <- srt[k] else hi <- srt[k]
      }
    } else if (!is.null(se)) {
      z <- .w3_nppf(1 - 0.5 * (1 - level))
      lo <- est - z * se
      hi <- est + z * se
    }
  }

  out <- list(anomaly = anom, variance = bl$variance,
              land_weight = bl$land_weight, observed = seen,
              estimate = est, se = se, se_uncorrelated = se_unc,
              se_correlated = se_cor, se_coverage = se_cov,
              ci_lower = lo, ci_upper = hi, level = level,
              north = agg$north, south = agg$south, n_north = agg$n_north,
              n_south = agg$n_south, n_observed = n_obs,
              n_cells = n_lat * n_lon,
              coverage = if (tot > 0) got / tot else NaN,
              rule = rule, route = route, interval = interval,
              method = "HadCRUT5 blended anomaly")
  # `$<-` with NULL DELETES the element instead of storing it, so the
  # optional pieces have to go in through single-bracket assignment or
  # the key vanishes from the payload whenever it is absent.
  out["coverage_draws"] <- list(cov_draws)
  out["ensemble_means"] <- list(members)
  out
}

#' One-line summary of the hadcrut module
#'
#' @return A character scalar.
#' @export
morie_hadcrut_cheatsheet <- function()
  paste0("hadcrut: HadCRUT5 blended land/SST anomaly. rules ",
         paste(.HADCRUT_WEIGHT_RULES, collapse = ", "), "; routes ",
         paste(.HADCRUT_MEAN_ROUTES, collapse = ", "), "; intervals ",
         paste(.HADCRUT_INTERVALS, collapse = ", "))
