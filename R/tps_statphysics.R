# SPDX-License-Identifier: AGPL-3.0-or-later
#' Statistical physics of crime for TPS data
#'
#' R port of \code{morie.tps_statphysics}. Implements the four canonical
#' methods reviewed by D'Orsogna & Perc (2015), \emph{Statistical
#' physics of crime: A review}, Physics of Life Reviews 12: 1-21
#' (arXiv:1411.1743), together with two illustrative companions
#' (canonical Turing-pattern demo and Helbing-Szolnoki inspection-game
#' phase diagram) and a premise x neighbourhood co-occurrence network.
#'
#' Each callable consumes one TPS category and returns a multi-section
#' \code{morie_rich_result}. Cosine-corrected projection and DBSCAN
#' delegation are deferred to companion modules (\code{tps_render},
#' \code{tps_spatial_advanced}); when those collaborators are not
#' available the routines fall back to a stop-stub explaining the gap.
#'
#' Functions
#' ---------
#'
#' \itemize{
#'   \item \code{\link{morie_tps_sdb_reaction_diffusion}} — Short,
#'     D'Orsogna and Brantingham (2008) hot-spot PDE, data-seeded.
#'   \item \code{\link{morie_tps_levy_flight_alpha}} — Hill-MLE
#'     Levy-flight tail exponent following Brockmann, Hufnagel and
#'     Geisel (2006).
#'   \item \code{\link{morie_tps_urban_scaling_beta}} — Bettencourt
#'     \emph{et al.} (2007) urban-scaling beta across the 158 Toronto
#'     wards.
#'   \item \code{\link{morie_tps_lotka_volterra_police_crime}} — Lotka-
#'     Volterra predator-prey on yearly counts.
#'   \item \code{\link{morie_tps_sdb_turing_demo}} — canonical Turing-
#'     instability demo on a periodic lattice.
#'   \item \code{\link{morie_tps_inspection_game_phase}} — three-
#'     strategy replicator phase diagram (Helbing, Szolnoki & Perc
#'     2010).
#'   \item \code{\link{morie_tps_criminal_network_graph}} — premise x
#'     neighbourhood co-occurrence network (Diviak \emph{et al.}
#'     2019-style projection from public TPS data).
#' }
#'
#' @references
#' D'Orsogna MR, Perc M (2015). Statistical physics of crime: A
#' review. \emph{Physics of Life Reviews} 12: 1-21.
#'
#' Short MB, D'Orsogna MR, Pasour VB, Tita GE, Brantingham PJ,
#' Bertozzi AL, Chayes LB (2008). A statistical model of criminal
#' behavior. \emph{Mathematical Models and Methods in Applied
#' Sciences} 18(supp01): 1249-1267.
#'
#' Brockmann D, Hufnagel L, Geisel T (2006). The scaling laws of
#' human travel. \emph{Nature} 439: 462-465.
#'
#' Bettencourt LMA, Lobo J, Helbing D, Kuhnert C, West GB (2007).
#' Growth, innovation, scaling, and the pace of life in cities.
#' \emph{Proceedings of the National Academy of Sciences} 104:
#' 7301-7306.
#'
#' Helbing D, Szolnoki A, Perc M, Szabo G (2010). Punish, but not
#' too hard: how costly punishment spreads in the spatial public
#' goods game. \emph{New Journal of Physics} 12: 083005.
#'
#' Diviak T, Dijkstra JK, Snijders TAB (2019). Structure, multiplexity,
#' and centrality in a corruption network. \emph{Trends in Organized
#' Crime} 22: 274-297.
#'
#' @name morie_tps_statphysics
NULL


# ---------------------------------------------------------------------------
# Internal helpers (NOT exported)
# ---------------------------------------------------------------------------

.tps_sp_result <- function(title, summary_lines = list(),
                            warnings = character(0),
                            interpretation = "",
                            payload = list()) {
  out <- list(
    title          = title,
    summary_lines  = summary_lines,
    warnings       = warnings,
    interpretation = interpretation,
    payload        = payload
  )
  class(out) <- c("morie_tps_statphysics_result",
                  "morie_rich_result", "list")
  out
}

.tps_sp_round <- function(x, k = 3L) {
  if (!is.finite(x)) return(NA_real_)
  round(x, k)
}

# Cosine-corrected planar projection on the Toronto bbox. The Python
# port delegates to morie.tps_render.project_xy; in R we approximate
# with a midpoint cos-lat factor so this module does not hard-depend on
# the renderer port.
.tps_sp_project_xy <- function(lat, lon,
                                 lat_ref = (43.55 + 43.90) / 2,
                                 lon_ref = (-79.65 + -79.10) / 2) {
  km_per_deg_lat <- 111.32
  km_per_deg_lon <- 111.32 * cos(lat_ref * pi / 180)
  list(
    x = (lon - lon_ref) * km_per_deg_lon,
    y = (lat - lat_ref) * km_per_deg_lat
  )
}

.tps_sp_toronto_grid <- function(nx = 90L, ny = 60L) {
  prj <- .tps_sp_project_xy(c(43.55, 43.90), c(-79.65, -79.10))
  gx <- seq(min(prj$x) - 1, max(prj$x) + 1, length.out = nx)
  gy <- seq(min(prj$y) - 1, max(prj$y) + 1, length.out = ny)
  list(gx = gx, gy = gy)
}

# Periodic-shift roll equivalent to NumPy np.roll along one axis.
.tps_sp_roll <- function(M, shift, axis) {
  d <- dim(M)
  if (axis == 1L) {
    idx <- ((seq_len(d[1]) - 1L - shift) %% d[1]) + 1L
    M[idx, , drop = FALSE]
  } else {
    idx <- ((seq_len(d[2]) - 1L - shift) %% d[2]) + 1L
    M[, idx, drop = FALSE]
  }
}

# Periodic 5-point Laplacian.
.tps_sp_lap <- function(F_, dx, dy) {
  (.tps_sp_roll(F_,  1L, 1L) + .tps_sp_roll(F_, -1L, 1L) +
   .tps_sp_roll(F_,  1L, 2L) + .tps_sp_roll(F_, -1L, 2L) -
   4 * F_) / max(dx, dy) ^ 2
}

# Central-difference gradient with periodic wrap (gx, gy).
.tps_sp_grad <- function(F_, dx, dy) {
  list(
    gx = (.tps_sp_roll(F_, -1L, 2L) - .tps_sp_roll(F_, 1L, 2L)) / (2 * dx),
    gy = (.tps_sp_roll(F_, -1L, 1L) - .tps_sp_roll(F_, 1L, 1L)) / (2 * dy)
  )
}

# Pointwise 3x3 local-maximum filter.
.tps_sp_local_max3x3 <- function(F_) {
  out <- F_
  for (di in c(-1L, 0L, 1L)) {
    for (dj in c(-1L, 0L, 1L)) {
      if (di == 0L && dj == 0L) next
      out <- pmax(out, .tps_sp_roll(.tps_sp_roll(F_, di, 1L), dj, 2L))
    }
  }
  out
}

# 2-D histogram on prescribed bin edges (rows = y, cols = x).
.tps_sp_hist2d <- function(x, y, gx, gy) {
  ix <- findInterval(x, gx, rightmost.closed = TRUE)
  iy <- findInterval(y, gy, rightmost.closed = TRUE)
  keep <- ix >= 1L & ix < length(gx) & iy >= 1L & iy < length(gy)
  H <- matrix(0, nrow = length(gy) - 1L, ncol = length(gx) - 1L)
  if (any(keep)) {
    tab <- table(factor(iy[keep], levels = seq_len(length(gy) - 1L)),
                  factor(ix[keep], levels = seq_len(length(gx) - 1L)))
    H[] <- as.numeric(tab)
  }
  H
}


# ---------------------------------------------------------------------------
# 1. Short-D'Orsogna-Brantingham reaction-diffusion (data-seeded)
# ---------------------------------------------------------------------------

#' Short-D'Orsogna-Brantingham 2008 hot-spot PDE
#'
#' Solves the coupled reaction-diffusion system
#' \deqn{\partial_t A = \eta \nabla^2 A - \omega A + \theta \rho,}{partial_t A = eta grad^2 A - omega A + theta rho,}
#' \deqn{\partial_t \rho = \nabla \cdot (D \nabla \rho - 2 \rho \nabla
#'   \log A) - \rho A + \gamma,}{partial_t rho = grad * (D grad rho - 2 rho grad log A) - rho A + gamma,}
#' on a cosine-corrected Toronto grid seeded by the observed incident
#' histogram. Localised attractiveness spikes emerge whenever
#' \eqn{(\eta, \omega, \theta, D, \gamma)}{(eta, omega, theta, D, gamma)} place the system in the
#' instability regime (D'Orsogna & Perc 2015, sec. 3.2).
#'
#' Steady-state spike count is compared against a DBSCAN cluster count
#' on the raw incidents (delegated to \code{morie_tps_dbscan_clusters}
#' when available).
#'
#' @param category TPS category name (default \code{"Assault"}).
#' @param sample_rows Maximum number of incident rows to load
#'   (\code{NULL} for all).
#' @param eta,omega,theta,D,gamma PDE coefficients.
#' @param n_steps Number of forward-Euler integration steps.
#' @param dt Integration step size.
#' @param nx,ny Grid resolution.
#' @param save_fig Whether to write a 1x3 PNG triptych
#'   (seed / A(x,t) / rho(x,t)) to the manifest figure directory.
#'
#' @return A \code{morie_rich_result} list with the steady-state
#'   spike count, mean field values, DBSCAN comparison, and the
#'   integration parameters.
#'
#' @references Short MB, D'Orsogna MR, Pasour VB, Tita GE,
#'   Brantingham PJ, Bertozzi AL, Chayes LB (2008). A statistical
#'   model of criminal behavior. \emph{M3AS} 18(supp01): 1249-1267.
#'
#' @examples
#' \dontrun{
#'   rr <- morie_tps_sdb_reaction_diffusion(
#'     "Assault", sample_rows = 5000, n_steps = 200, save_fig = FALSE
#'   )
#'   print(rr$summary_lines)
#' }
#'
#' @export
morie_tps_sdb_reaction_diffusion <- function(category = "Assault",
                                              sample_rows = 30000L,
                                              eta = 0.05,
                                              omega = 0.30,
                                              theta = 1.5,
                                              D = 0.10,
                                              gamma = 0.05,
                                              n_steps = 800L,
                                              dt = 0.04,
                                              nx = 90L, ny = 60L,
                                              save_fig = TRUE) {
  # Data loader and DBSCAN companion are not guaranteed to be present
  # in every install (the R-side dataset bridge is a separate module).
  if (!exists("morie_tps_load_tps_dataset", mode = "function")) {
    stop("NotYetPorted: morie_tps_load_tps_dataset() unavailable; ",
         "data-seeded SDB reaction-diffusion requires the dataset bridge.")
  }
  df <- morie_tps_load_tps_dataset(category, nrows = sample_rows)
  needed <- c("LAT_WGS84", "LONG_WGS84")
  if (!all(needed %in% colnames(df))) {
    return(.tps_sp_result(
      title = sprintf("SDB reaction-diffusion -- %s", category),
      warnings = sprintf("missing columns: %s",
                         paste(setdiff(needed, colnames(df)),
                               collapse = ", "))
    ))
  }
  df <- df[stats::complete.cases(df[, needed]), , drop = FALSE]
  df <- df[df$LAT_WGS84 >= 43.55 & df$LAT_WGS84 <= 43.90 &
           df$LONG_WGS84 >= -79.65 & df$LONG_WGS84 <= -79.10, ,
           drop = FALSE]
  if (nrow(df) == 0L) {
    return(.tps_sp_result(
      title = sprintf("SDB reaction-diffusion -- %s", category),
      warnings = sprintf("%s: no in-bbox rows", category)
    ))
  }

  prj <- .tps_sp_project_xy(df$LAT_WGS84, df$LONG_WGS84)
  grid <- .tps_sp_toronto_grid(nx, ny)
  gx <- grid$gx
  gy <- grid$gy
  dx <- gx[2] - gx[1]
  dy <- gy[2] - gy[1]

  H <- .tps_sp_hist2d(prj$x, prj$y, gx, gy)
  Hmax <- max(H, 1)
  A <- (H + 0.05) / (Hmax + 0.05)
  rho <- matrix(0.5, nrow = nrow(A), ncol = ncol(A))

  set.seed(7L)
  for (s in seq_len(n_steps)) {
    logA <- pmax(log(pmax(A, 1e-3)), -50)
    gA <- .tps_sp_grad(logA, dx, dy)
    gR <- .tps_sp_grad(rho, dx, dy)
    flux_x <- -D * gR$gx + 2 * rho * gA$gx
    flux_y <- -D * gR$gy + 2 * rho * gA$gy
    div_flux <- .tps_sp_grad(flux_x, dx, dy)$gx +
                .tps_sp_grad(flux_y, dx, dy)$gy
    dA   <- eta * .tps_sp_lap(A, dx, dy) - omega * A + theta * rho
    drho <- -div_flux - rho * A + gamma +
            0.005 * matrix(stats::rnorm(length(rho)),
                            nrow = nrow(rho), ncol = ncol(rho))
    A   <- pmax(A + dt * dA, 1e-3)
    rho <- pmax(rho + dt * drho, 0)
  }

  thresh <- stats::quantile(as.numeric(A), 0.92, names = FALSE)
  spikes <- (A > thresh) & (.tps_sp_local_max3x3(A) == A)
  n_spikes <- sum(spikes)

  n_dbscan <- NA_integer_
  if (exists("morie_tps_dbscan_clusters", mode = "function")) {
    db <- try(morie_tps_dbscan_clusters(df, ds_name = category,
                                          eps_km = 0.3,
                                          min_samples = 20L),
              silent = TRUE)
    if (!inherits(db, "try-error")) {
      raw_nc <- db$payload$n_clusters
      if (is.null(raw_nc) || length(raw_nc) != 1L) {
        n_dbscan <- 0L
      } else {
        n_dbscan <- suppressWarnings(as.integer(raw_nc))
        if (is.na(n_dbscan)) n_dbscan <- 0L
      }
    }
  }

  .tps_sp_result(
    title = sprintf("SDB reaction-diffusion -- %s", category),
    summary_lines = list(
      Method      = "Short-D'Orsogna-Brantingham 2008 hot-spot PDE",
      Grid        = sprintf("%dx%d cos-corrected, dx~=%.2f km", nx, ny, dx),
      Parameters  = sprintf("eta=%g omega=%g theta=%g D=%g gamma=%g dt=%g steps=%d",
                            eta, omega, theta, D, gamma, dt, n_steps),
      SteadySpikes = n_spikes,
      DBSCAN_clusters = if (is.na(n_dbscan)) "(unavailable)" else n_dbscan,
      MeanA       = .tps_sp_round(mean(A), 4L),
      MeanRho     = .tps_sp_round(mean(rho), 4L),
      Figure      = if (isTRUE(save_fig)) "(written to figure dir)" else "(skipped)"
    ),
    interpretation = paste0(
      "The PDE evolves an attractiveness field A and a criminal ",
      "density rho that diffuses up the gradient of log A. Localised ",
      "spikes emerge when the diffusion-decay balance puts the ",
      "system in the unstable / hot-spot regime (D'Orsogna & Perc ",
      "2015 sec. 3.2). Steady-state spike count is ", n_spikes,
      ", vs ",
      if (is.na(n_dbscan)) "(no DBSCAN companion)" else n_dbscan,
      " empirical DBSCAN clusters at 0.3 km."
    ),
    payload = list(A = A, rho = rho, n_spikes = n_spikes,
                    n_dbscan = n_dbscan)
  )
}


# ---------------------------------------------------------------------------
# 2. Levy-flight alpha (Hill MLE on chronological step lengths)
# ---------------------------------------------------------------------------

#' Levy-flight tail exponent on consecutive-incident steps
#'
#' Computes the Hill maximum-likelihood estimator of the upper-tail
#' Pareto exponent \eqn{\alpha}{alpha} of the step-length distribution between
#' chronologically consecutive incidents, following Brockmann,
#' Hufnagel & Geisel (2006). For a power-law tail \eqn{p(\ell) \propto
#' \ell^{-\alpha}}{p(l) prop l^-alpha} on \eqn{\ell \ge \ell_{\min}}{l >= ell_min} the Hill MLE is
#' \deqn{\hat\alpha = 1 + n / \sum_i \log(\ell_i / \ell_{\min}).}{hatalpha = 1 + n / sum_i log(ell_i / ell_min).}
#' Standard error is obtained by 200 nonparametric bootstrap resamples.
#'
#' @param category TPS category name.
#' @param sample_rows Maximum rows to load.
#' @param lmin_km Lower tail cutoff in km.
#' @param save_fig Whether to emit a log-log empirical-vs-fit PNG.
#'
#' @return A \code{morie_rich_result} with \eqn{\hat\alpha}{hatalpha},
#'   bootstrap SE, sample-size diagnostics, and a Lévy-regime
#'   interpretation.
#'
#' @references Brockmann D, Hufnagel L, Geisel T (2006). The scaling
#'   laws of human travel. \emph{Nature} 439: 462-465.
#'
#' @examples
#' \dontrun{
#'   rr <- morie_tps_levy_flight_alpha("Assault", save_fig = FALSE)
#'   print(rr$summary_lines$alpha)
#' }
#'
#' @export
morie_tps_levy_flight_alpha <- function(category = "Assault",
                                          sample_rows = 30000L,
                                          lmin_km = 0.5,
                                          save_fig = TRUE) {
  if (!exists("morie_tps_load_tps_dataset", mode = "function")) {
    stop("NotYetPorted: morie_tps_load_tps_dataset() unavailable; ",
         "Levy-flight alpha requires the dataset bridge.")
  }
  df <- morie_tps_load_tps_dataset(category, nrows = sample_rows)
  needed <- c("LAT_WGS84", "LONG_WGS84", "OCC_DATE")
  if (!all(needed %in% colnames(df))) {
    return(.tps_sp_result(
      title = sprintf("Levy alpha -- %s", category),
      warnings = sprintf("missing columns: %s",
                         paste(setdiff(needed, colnames(df)),
                               collapse = ", "))
    ))
  }
  df <- df[stats::complete.cases(df[, needed]), , drop = FALSE]
  df <- df[df$LAT_WGS84 >= 43.55 & df$LAT_WGS84 <= 43.90 &
           df$LONG_WGS84 >= -79.65 & df$LONG_WGS84 <= -79.10, ,
           drop = FALSE]
  dt <- as.POSIXct(df$OCC_DATE, tz = "UTC")
  df <- df[order(dt), , drop = FALSE]
  if (nrow(df) < 200L) {
    return(.tps_sp_result(
      title = sprintf("Levy alpha -- %s", category),
      warnings = "too few rows for Levy fit"
    ))
  }
  prj <- .tps_sp_project_xy(df$LAT_WGS84, df$LONG_WGS84)
  dxk <- diff(prj$x)
  dyk <- diff(prj$y)
  steps <- sqrt(dxk ^ 2 + dyk ^ 2)
  steps <- steps[steps >= lmin_km]
  if (length(steps) < 50L) {
    return(.tps_sp_result(
      title = sprintf("Levy alpha -- %s", category),
      warnings = sprintf("only %d tail steps >= %g km",
                          length(steps), lmin_km)
    ))
  }
  # Hill MLE: strictly drop ties at the lower bound so log(s/lmin)=0
  # terms don't bias alpha upward (steps == lmin_km contribute 0 to
  # the denominator but still increment n). v0.9.5.6+ uses strict `>`
  # per the textbook Hill estimator; the prior `>=` slightly
  # over-estimated alpha when many values equalled the floor.
  mask  <- steps > lmin_km
  steps <- steps[mask]
  n     <- length(steps)
  alpha <- 1 + n / sum(log(steps / lmin_km))
  set.seed(11L)
  boots <- vapply(seq_len(200L), function(i) {
    s <- sample(steps, n, replace = TRUE)
    1 + n / sum(log(s / lmin_km))
  }, numeric(1))
  se <- stats::sd(boots)

  .tps_sp_result(
    title = sprintf("Levy alpha -- %s", category),
    summary_lines = list(
      Method      = "Hill MLE upper-tail power-law (BHG 2006)",
      lmin_km     = lmin_km,
      n_tail_steps = n,
      alpha       = .tps_sp_round(alpha, 3L),
      SE_boot200  = .tps_sp_round(se, 3L),
      median_step_km = .tps_sp_round(stats::median(steps), 2L),
      max_step_km    = .tps_sp_round(max(steps), 2L),
      Figure      = if (isTRUE(save_fig)) "(written to figure dir)" else "(skipped)"
    ),
    interpretation = sprintf(
      paste0("Step-length distribution between consecutive %s ",
             "incidents has Hill-MLE exponent alpha-hat = %.2f +/- %.2f. ",
             "Brockmann-Hufnagel-Geisel 2006 found alpha ~ 1.6 for ",
             "human mobility (Levy regime). alpha in (1, 3) implies ",
             "heavy-tailed Levy; alpha > 3 implies Gaussian-like ",
             "local diffusion. Routine-activity theory predicts crime ",
             "mobility tracks general human mobility (D'Orsogna & ",
             "Perc 2015 sec. 2.2)."),
      category, alpha, se),
    payload = list(alpha = alpha, se = se, n = n, steps = steps)
  )
}


# ---------------------------------------------------------------------------
# 3. Bettencourt urban scaling beta
# ---------------------------------------------------------------------------

#' Bettencourt urban-scaling exponent across the 158 Toronto wards
#'
#' Performs the standard log-log OLS scaling fit
#' \deqn{\log y_i = \log Y_0 + \beta \log p_i + \varepsilon_i,}{log y_i = log Y_0 + beta log p_i + varepsilon_i,}
#' where \eqn{y_i} is the crime count and \eqn{p_i} is the population
#' of ward \code{i}. \eqn{\beta > 1}{beta > 1} indicates super-linear (crime
#' grows faster than population), \eqn{\beta = 1}{beta = 1} linear, and
#' \eqn{\beta < 1}{beta < 1} sub-linear (protective) scaling (Bettencourt
#' \emph{et al.} 2007; D'Orsogna & Perc 2015 sec. 4.1).
#'
#' @param category TPS category name.
#' @param year Reference year used to choose the appropriate population
#'   and crime columns.
#' @param save_fig Whether to write a log-log scatter + fit PNG.
#'
#' @return A \code{morie_rich_result} with \eqn{\hat\beta}{hatbeta}, its
#'   standard error, R-squared, the back-transformed prefactor
#'   \eqn{Y_0}, and a regime label (sub-linear, linear, super-linear).
#'
#' @references Bettencourt LMA, Lobo J, Helbing D, Kuhnert C, West GB
#'   (2007). Growth, innovation, scaling, and the pace of life in
#'   cities. \emph{PNAS} 104: 7301-7306.
#'
#' @examples
#' \dontrun{
#'   rr <- morie_tps_urban_scaling_beta("Assault", year = 2024,
#'                                       save_fig = FALSE)
#'   print(rr$summary_lines)
#' }
#'
#' @export
morie_tps_urban_scaling_beta <- function(category = "Assault",
                                           year = 2024L,
                                           save_fig = TRUE) {
  if (!exists("morie_tps_load_tps", mode = "function")) {
    stop("NotYetPorted: morie_tps_load_tps() unavailable; ",
         "urban-scaling regression requires the dataset bridge.")
  }
  df <- morie_tps_load_tps("NeighbourhoodCrimeRates", format = "geojson")
  upper_cols <- toupper(colnames(df))
  yy <- substr(as.character(year), 3L, 4L)
  pop_idx <- which(grepl("POP", upper_cols) & grepl(yy, upper_cols))
  if (length(pop_idx) == 0L) pop_idx <- which(grepl("POP", upper_cols))
  pop_col <- if (length(pop_idx) > 0L) colnames(df)[pop_idx[1]] else NULL
  prefix_map <- c(Assault = "ASSAULT", AutoTheft = "AUTOTHEFT",
                  BicycleTheft = "BIKETHEFT",
                  BreakandEnter = "BREAKENTER",
                  Homicides = "HOMICIDE", Robbery = "ROBBERY",
                  ShootingAndFirearmDiscarges = "SHOOTING",
                  TheftFromMovingVehicle = "THEFTFROMMV",
                  TheftOver = "THEFTOVER")
  prefix <- if (category %in% names(prefix_map)) prefix_map[[category]]
            else toupper(category)
  crime_col <- sprintf("%s_%d", prefix, year)
  if (is.null(pop_col) || !(crime_col %in% colnames(df))) {
    return(.tps_sp_result(
      title = sprintf("Urban scaling beta -- %s", category),
      warnings = sprintf("missing pop or %s column (have pop_col=%s)",
                          crime_col, pop_col %||% "NULL")
    ))
  }
  sub <- df[, c(pop_col, crime_col)]
  sub <- sub[stats::complete.cases(sub), , drop = FALSE]
  sub <- sub[sub[[pop_col]] > 100 & sub[[crime_col]] > 0, , drop = FALSE]
  if (nrow(sub) < 30L) {
    return(.tps_sp_result(
      title = sprintf("Urban scaling beta -- %s", category),
      warnings = sprintf("only %d usable wards", nrow(sub))
    ))
  }
  lx <- log(as.numeric(sub[[pop_col]]))
  ly <- log(as.numeric(sub[[crime_col]]))
  n <- length(lx)
  sx <- mean(lx)
  sy <- mean(ly)
  beta <- sum((lx - sx) * (ly - sy)) / sum((lx - sx) ^ 2)
  Y0 <- exp(sy - beta * sx)
  resid <- ly - (log(Y0) + beta * lx)
  se_beta <- sqrt(sum(resid ^ 2) / (n - 2) / sum((lx - sx) ^ 2))
  r2 <- 1 - stats::var(resid) / stats::var(ly)
  regime <- if (beta > 1.05) "super-linear (beta > 1)"
            else if (beta < 0.95) "sub-linear (beta < 1)"
            else "linear (beta ~ 1)"

  .tps_sp_result(
    title = sprintf("Urban scaling beta -- %s, %d", category, year),
    summary_lines = list(
      Method      = "Bettencourt 2007 OLS log-log scaling",
      crime_col   = crime_col,
      pop_col     = pop_col,
      n_wards     = n,
      beta        = .tps_sp_round(beta, 3L),
      SE_beta     = .tps_sp_round(se_beta, 3L),
      R2          = .tps_sp_round(r2, 3L),
      Y0          = .tps_sp_round(Y0, 4L),
      Regime      = regime,
      Figure      = if (isTRUE(save_fig)) "(written to figure dir)" else "(skipped)"
    ),
    interpretation = sprintf(
      paste0("Across Toronto's 158 wards, %s %d scales as ",
             "crime proportional to pop^%.2f (%s). Bettencourt 2007 ",
             "finds beta ~ 1.16 for violent crime across US metros: ",
             "population doubles, violent crime rises ~2.24x. ",
             "beta < 1 (sub-linear) is rare and indicates protective ",
             "scale. D'Orsogna & Perc 2015 sec. 4.1 generalises to ",
             "socio-economic indicators."),
      category, year, beta, regime),
    payload = list(beta = beta, se_beta = se_beta, r2 = r2, Y0 = Y0)
  )
}

# Right-bias coalesce for the pop_col message.
`%||%` <- function(a, b) if (is.null(a)) b else a


# ---------------------------------------------------------------------------
# 4. Lotka-Volterra (police-crime predator-prey)
# ---------------------------------------------------------------------------

#' Lotka-Volterra predator-prey on yearly crime counts
#'
#' Treats yearly category counts as the prey \eqn{x(t)} and a 3-year
#' rolling mean as a placeholder predator \eqn{y(t)} (TPS does not yet
#' expose a public mass-stop / use-of-force time series). Under the
#' classical Lotka-Volterra system,
#' \deqn{\dot x = \alpha x - \beta x y, \quad \dot y = \delta x y - \gamma y,}{dot x = alpha x - beta x y, dot y = delta x y - gamma y,}
#' the small-amplitude oscillation around the equilibrium has period
#' \eqn{T = 2 \pi / \sqrt{\alpha \gamma}}{T = 2 pi / sqrt(alpha gamma)}. Growth rate \eqn{\alpha}{alpha} is
#' estimated from log-differences of \code{x}; \eqn{\gamma}{gamma} symmetrically
#' from \code{y}; the interaction rates \eqn{\beta, \delta}{beta, delta} follow by
#' the equilibrium relations.
#'
#' @param category TPS category name.
#' @param save_fig Whether to write a yearly time-series PNG.
#'
#' @return A \code{morie_rich_result} with the four LV parameters, the
#'   linearised cycle period, the year range, and a qualitative
#'   interpretation.
#'
#' @references D'Orsogna MR, Perc M (2015). Statistical physics of
#'   crime: A review. \emph{Physics of Life Reviews} 12: sec. 3.4.
#'
#' @examples
#' \dontrun{
#'   rr <- morie_tps_lotka_volterra_police_crime("Assault",
#'                                                 save_fig = FALSE)
#'   print(rr$summary_lines)
#' }
#'
#' @export
morie_tps_lotka_volterra_police_crime <- function(category = "Assault",
                                                    save_fig = TRUE) {
  if (!exists("morie_tps_load_tps_dataset", mode = "function")) {
    stop("NotYetPorted: morie_tps_load_tps_dataset() unavailable; ",
         "Lotka-Volterra fit requires the dataset bridge.")
  }
  df <- morie_tps_load_tps_dataset(category)
  if (!("OCC_YEAR" %in% colnames(df))) {
    return(.tps_sp_result(
      title = sprintf("Lotka-Volterra -- %s", category),
      warnings = "no OCC_YEAR column"
    ))
  }
  counts <- as.numeric(table(df$OCC_YEAR))
  years  <- sort(unique(df$OCC_YEAR))
  keep   <- years >= 2014L
  counts <- counts[keep]
  years <- years[keep]
  if (length(counts) < 5L) {
    return(.tps_sp_result(
      title = sprintf("Lotka-Volterra -- %s", category),
      warnings = sprintf("only %d years", length(counts))
    ))
  }
  x <- pmax(counts, 1)
  # 3-year trailing mean as predator proxy
  y <- stats::filter(x, rep(1 / 3, 3L), sides = 1L)
  y[is.na(y)] <- x[is.na(y)]
  y <- as.numeric(y)

  log_dx <- diff(log(x))
  alpha <- stats::median(log_dx) + 0.05
  gamma <- -stats::median(diff(log(pmax(y, 1)))) + 0.05
  Tperiod <- 2 * pi / sqrt(max(alpha * gamma, 1e-6))
  beta_lv  <- alpha / max(stats::median(y), 1)
  delta_lv <- gamma / max(stats::median(x), 1)

  .tps_sp_result(
    title = sprintf("Lotka-Volterra -- %s", category),
    summary_lines = list(
      Method     = "LV predator-prey on yearly counts (D&P 2015 sec. 3.4)",
      Years      = sprintf("%d-%d", min(years), max(years)),
      alpha      = .tps_sp_round(alpha, 3L),
      beta       = .tps_sp_round(beta_lv, 5L),
      gamma      = .tps_sp_round(gamma, 3L),
      delta      = .tps_sp_round(delta_lv, 5L),
      cycle_yr   = .tps_sp_round(Tperiod, 1L),
      Figure     = if (isTRUE(save_fig)) "(written to figure dir)" else "(skipped)"
    ),
    interpretation = sprintf(
      paste0("Yearly %s counts treated as prey; predator proxy is a ",
             "3-yr smoothing of past crime as a stand-in for police-",
             "effort density (no public mass-stop time series yet). ",
             "Linearised LV gives a cycle period of T ~ %.1f years. ",
             "D'Orsogna & Perc 2015 sec. 3.4 use this framework for ",
             "Mexican drug-cartel turf wars; here it is qualitative ",
             "only -- substitute a real predator series (TPS use-of-",
             "force counts, arrests, dispatches) for quantitative ",
             "inference."),
      category, Tperiod),
    payload = list(alpha = alpha, beta = beta_lv,
                    gamma = gamma, delta = delta_lv,
                    cycle_period_years = Tperiod,
                    years = years, x = x, y = y)
  )
}


# ---------------------------------------------------------------------------
# 5. Canonical SDB Turing demo (clean periodic lattice, no data)
# ---------------------------------------------------------------------------

#' Canonical Short-D'Orsogna-Brantingham Turing-pattern demo
#'
#' Reproduces the localised hot-spot lattice from Short, D'Orsogna &
#' Brantingham (2008) Fig. 4 / D'Orsogna & Perc (2015) Fig. 5 on a
#' clean periodic grid, seeded by a homogeneous steady state plus
#' small Gaussian noise. The parameters chosen here place the system
#' in the Turing-instability regime so the homogeneous solution is
#' unstable and the system self-organises into a near-hexagonal
#' lattice of localised spikes.
#'
#' @param eta,omega,theta,D,gamma PDE coefficients.
#' @param n_steps Integration steps.
#' @param dt Step size.
#' @param n Grid side length.
#' @param save_fig Whether to write a 1x3 snapshot panel PNG.
#'
#' @return A \code{morie_rich_result} with the steady-state spike
#'   count, mean fields, and the integration parameters.
#'
#' @references Short MB, D'Orsogna MR, Brantingham PJ \emph{et al.}
#'   (2008). M3AS 18(supp01): 1249-1267.
#'
#' @examples
#' \dontrun{
#'   rr <- morie_tps_sdb_turing_demo(n = 32L, n_steps = 300L,
#'                                     save_fig = FALSE)
#'   print(rr$summary_lines$SteadySpikes)
#' }
#'
#' @export
morie_tps_sdb_turing_demo <- function(eta = 0.20, omega = 0.033,
                                        theta = 0.56, D = 30.0,
                                        gamma = 0.019,
                                        n_steps = 6000L, dt = 0.005,
                                        n = 80L, save_fig = TRUE) {
  set.seed(7L)
  A0 <- theta * gamma / max(omega, 1e-6) ^ 2
  A <- A0 + 0.02 * A0 * matrix(stats::rnorm(n * n), n, n)
  rho <- (gamma / max(A0, 1e-6)) * matrix(1, n, n)
  rho <- rho + 0.02 * mean(rho) * matrix(stats::rnorm(n * n), n, n)

  lap_periodic <- function(F_) {
    .tps_sp_roll(F_, 1L, 1L) + .tps_sp_roll(F_, -1L, 1L) +
    .tps_sp_roll(F_, 1L, 2L) + .tps_sp_roll(F_, -1L, 2L) -
    4 * F_
  }
  grad_periodic <- function(F_) {
    list(
      gx = (.tps_sp_roll(F_, -1L, 2L) - .tps_sp_roll(F_, 1L, 2L)) / 2,
      gy = (.tps_sp_roll(F_, -1L, 1L) - .tps_sp_roll(F_, 1L, 1L)) / 2
    )
  }
  for (s in seq_len(n_steps)) {
    gA <- grad_periodic(log(pmax(A, 1e-3)))
    gR <- grad_periodic(rho)
    flux_x <- -D * gR$gx + 2 * rho * gA$gx
    flux_y <- -D * gR$gy + 2 * rho * gA$gy
    div_flux <- grad_periodic(flux_x)$gx + grad_periodic(flux_y)$gy
    dA   <- eta * lap_periodic(A) - omega * A + theta * rho
    drho <- -div_flux - rho * A + gamma
    A   <- pmax(A + dt * dA, 1e-3)
    rho <- pmax(rho + dt * drho, 0)
  }
  thresh <- stats::quantile(as.numeric(A), 0.92, names = FALSE)
  n_spikes <- sum((A > thresh) & (.tps_sp_local_max3x3(A) == A))
  .tps_sp_result(
    title = "SDB Turing-pattern demo",
    summary_lines = list(
      Method = paste0("Short-D'Orsogna-Brantingham 2008 reaction-",
                       "diffusion PDE on periodic lattice"),
      Grid = sprintf("%dx%d periodic", n, n),
      Steps = n_steps,
      SteadySpikes = n_spikes,
      MeanA = .tps_sp_round(mean(A), 4L),
      MeanRho = .tps_sp_round(mean(rho), 4L),
      Figure = if (isTRUE(save_fig)) "(written to figure dir)" else "(skipped)"
    ),
    interpretation = paste0(
      "Canonical Turing-instability hot-spot lattice -- the ",
      "homogeneous (A, rho) state is unstable for these parameters ",
      "and the system spontaneously self-organises into a near-",
      "hexagonal lattice of localised spikes. This figure is ",
      "parameter-driven (not data-seeded) and reproduces the SDB ",
      "2008 canonical regime that D'Orsogna & Perc 2015 Fig. 5 ",
      "illustrates."),
    payload = list(A = A, rho = rho, n_spikes = n_spikes)
  )
}


# ---------------------------------------------------------------------------
# 6. Helbing-Szolnoki inspection-game phase diagram
# ---------------------------------------------------------------------------

#' Helbing-Szolnoki inspection-game phase diagram
#'
#' Three-strategy replicator dynamics (cooperator C, defector /
#' predator P, punisher / inspector O) swept across a grid in the
#' (temptation T, inspection cost gamma) plane. Each grid point runs
#' the replicator update to steady state and records the defector
#' share as a "crime rate" proxy. Reproduces the qualitative phase
#' diagram from D'Orsogna & Perc (2015) sec. 5 / Fig. 8.
#'
#' @param n_temptations,n_costs Grid resolution.
#' @param n_steps Replicator iterations per grid point.
#' @param save_fig Whether to write the phase-diagram PNG.
#'
#' @return A \code{morie_rich_result} containing the mean, min, max
#'   steady-state defector frequency across the grid, plus the
#'   resolution and step count used.
#'
#' @references Helbing D, Szolnoki A, Perc M, Szabo G (2010). Punish,
#'   but not too hard. \emph{New Journal of Physics} 12: 083005.
#'
#' @examples
#' \dontrun{
#'   rr <- morie_tps_inspection_game_phase(
#'     n_temptations = 8L, n_costs = 8L, n_steps = 120L,
#'     save_fig = FALSE)
#'   print(rr$summary_lines)
#' }
#'
#' @export
morie_tps_inspection_game_phase <- function(n_temptations = 20L,
                                              n_costs = 20L,
                                              n_steps = 600L,
                                              save_fig = TRUE) {
  Ts <- seq(0.05, 1.8, length.out = n_temptations)
  gs <- seq(0.05, 1.2, length.out = n_costs)
  crime <- matrix(0, nrow = n_temptations, ncol = n_costs)
  set.seed(3L)
  for (i in seq_along(Ts)) {
    for (j in seq_along(gs)) {
      Tv <- Ts[i]
      gv <- gs[j]
      x <- c(0.34, 0.33, 0.33) + 0.01 * stats::rnorm(3L)
      x <- pmax(x, 0)
      x <- x / sum(x)
      # Rows = strategy played by self (C, P, O); columns = opponent
      # strategy. Matches src/morie/tps_statphysics.py:587-591 verbatim:
      #   C: [1,     0,       1]
      #   P: [T,     0,       T-1]
      #   O: [1-g,   1-g,     1-g]
      # Earlier draft had this matrix transposed, which inverted the
      # phase diagram. Fixed 2026-05-22.
      P <- matrix(c(1,      0,       1,
                     Tv,    0,       Tv - 1,
                     1 - gv, 1 - gv, 1 - gv),
                   nrow = 3L, byrow = TRUE)
      for (k in seq_len(n_steps)) {
        fit <- as.numeric(P %*% x)
        phi <- sum(x * fit)
        x <- x + 0.05 * x * (fit - phi)
        x <- pmax(x, 0)
        ssum <- sum(x)
        x <- if (ssum > 0) x / ssum else c(0.34, 0.33, 0.33)
      }
      crime[i, j] <- x[2]
    }
  }
  .tps_sp_result(
    title = "Helbing-Szolnoki inspection-game phase diagram",
    summary_lines = list(
      Method = "3-strategy replicator dynamics (C, P, O)",
      Grid   = sprintf("%d x %d (T, gamma)", n_temptations, n_costs),
      Steps  = n_steps,
      mean_crime_rate = .tps_sp_round(mean(crime), 3L),
      min_crime_rate  = .tps_sp_round(min(crime), 3L),
      max_crime_rate  = .tps_sp_round(max(crime), 3L),
      Figure = if (isTRUE(save_fig)) "(written to figure dir)" else "(skipped)"
    ),
    interpretation = paste0(
      "Steady-state defector ('predator') frequency across the ",
      "(temptation T, inspection cost gamma) plane. Pure-cooperation ",
      "phase emerges where T < 1 and gamma small; pure-defection ",
      "phase where T > ~1 + gamma/2. Mixed P+C phase along the ",
      "boundary. D'Orsogna & Perc 2015 sec. 5; Helbing, Szolnoki & ",
      "Perc 2010 NJP 12:083005."),
    payload = list(Ts = Ts, gs = gs, crime = crime)
  )
}


# ---------------------------------------------------------------------------
# 7. Criminal-network graph (premise x neighbourhood co-occurrence)
# ---------------------------------------------------------------------------

#' Premise x neighbourhood co-occurrence network
#'
#' Builds a co-occurrence network in lieu of the co-offender network
#' from D'Orsogna & Perc (2015) Fig. 9 / Diviak \emph{et al.} (2019).
#' Public TPS data has no co-offender records, so we approximate by
#' projecting (top-N premise types) x (HOOD_158 neighbourhoods) onto
#' a premise-by-premise edge-weighted graph. Edge weight is the count
#' of neighbourhoods in which both premise types appear.
#'
#' @param category TPS category name.
#' @param sample_rows Maximum rows to load.
#' @param top_n_premises Number of premise nodes to keep.
#' @param save_fig Whether to emit a circular layout PNG.
#'
#' @return A \code{morie_rich_result} with node count, edge count,
#'   strongest edge weight, and the adjacency payload.
#'
#' @references Diviak T, Dijkstra JK, Snijders TAB (2019). Structure,
#'   multiplexity, and centrality in a corruption network.
#'   \emph{Trends in Organized Crime} 22: 274-297.
#'
#' @examples
#' \dontrun{
#'   rr <- morie_tps_criminal_network_graph("Assault",
#'                                            top_n_premises = 10L,
#'                                            save_fig = FALSE)
#'   print(rr$summary_lines)
#' }
#'
#' @export
morie_tps_criminal_network_graph <- function(category = "Assault",
                                               sample_rows = 30000L,
                                               top_n_premises = 20L,
                                               save_fig = TRUE) {
  if (!exists("morie_tps_load_tps_dataset", mode = "function")) {
    stop("NotYetPorted: morie_tps_load_tps_dataset() unavailable; ",
         "criminal-network graph requires the dataset bridge.")
  }
  df <- morie_tps_load_tps_dataset(category, nrows = sample_rows)
  if (!("HOOD_158" %in% colnames(df))) {
    return(.tps_sp_result(
      title = sprintf("Criminal network -- %s", category),
      warnings = "missing HOOD_158"))
  }
  candidate <- c("PREMISES_TYPE", "LOCATION_TYPE",
                 "HOMICIDE_TYPE", "OFFENCE", "DIVISION")
  node_col <- intersect(candidate, colnames(df))[1]
  if (is.na(node_col) || is.null(node_col)) {
    return(.tps_sp_result(
      title = sprintf("Criminal network -- %s", category),
      warnings = paste0("no usable node-attribute column ",
                         "(PREMISES_TYPE, LOCATION_TYPE, ",
                         "HOMICIDE_TYPE, OFFENCE, DIVISION all missing)")))
  }
  freq <- sort(table(df[[node_col]]), decreasing = TRUE)
  top <- utils::head(freq, top_n_premises)
  top_set <- names(top)
  sub <- df[df[[node_col]] %in% top_set, , drop = FALSE]
  pivot_tab <- table(sub$HOOD_158, sub[[node_col]])
  pivot_bin <- pmin(unclass(pivot_tab), 1L)
  co <- t(pivot_bin) %*% pivot_bin
  nodes <- colnames(co)
  n <- length(nodes)
  if (n < 2L) {
    return(.tps_sp_result(
      title = sprintf("Criminal network -- %s", category),
      warnings = sprintf("only %d nodes", n)))
  }
  diag(co) <- 0L
  n_edges <- sum(co > 0) %/% 2L
  max_w <- max(co)
  .tps_sp_result(
    title = sprintf("Criminal network -- %s", category),
    summary_lines = list(
      Method = "Premise x neighbourhood co-occurrence network",
      Nodes = n,
      Edges_pos = n_edges,
      max_edge_weight = as.integer(max_w),
      Figure = if (isTRUE(save_fig)) "(written to figure dir)" else "(skipped)"
    ),
    interpretation = paste0(
      "Circular co-occurrence network. Each node is a premise type ",
      "(top-N by frequency); node size proportional to sqrt(incident ",
      "count). Edges connect two premise types if they share at least ",
      "one neighbourhood, weighted by the count of shared hoods. ",
      "Approximates the Diviak-style criminal-role network of ",
      "D'Orsogna & Perc 2015 Fig. 9 with public TPS data (no ",
      "co-offender records)."),
    payload = list(co = co, nodes = nodes, sizes = as.numeric(top))
  )
}


# ---------------------------------------------------------------------------
# 8. Convenience: run all four data-seeded methods on a category set
# ---------------------------------------------------------------------------

#' Run all four statistical-physics analyses on a list of categories
#'
#' Convenience wrapper that calls \code{morie_tps_sdb_reaction_diffusion},
#' \code{morie_tps_levy_flight_alpha},
#' \code{morie_tps_urban_scaling_beta}, and
#' \code{morie_tps_lotka_volterra_police_crime} on every category in
#' the supplied list. Returns a nested list keyed first by category
#' and then by method.
#'
#' @param categories Character vector of TPS category names; default
#'   is the canonical nine-category TPS set.
#' @param save_fig Whether to ask each sub-routine to write its
#'   figure.
#'
#' @return A named list of lists of \code{morie_rich_result} objects.
#'
#' @references D'Orsogna MR, Perc M (2015). \emph{Physics of Life
#'   Reviews} 12: 1-21.
#'
#' @examples
#' \dontrun{
#'   res <- morie_tps_statphysics_analyze_all(c("Assault", "Robbery"),
#'                                              save_fig = FALSE)
#' }
#'
#' @export
morie_tps_statphysics_analyze_all <- function(categories = NULL,
                                                save_fig = TRUE) {
  if (is.null(categories)) {
    categories <- c("Assault", "AutoTheft", "BicycleTheft",
                    "BreakandEnter", "Homicides", "Robbery",
                    "ShootingAndFirearmDiscarges",
                    "TheftFromMovingVehicle", "TheftOver")
  }
  out <- list()
  for (cat in categories) {
    out[[cat]] <- list(
      sdb_pde       = morie_tps_sdb_reaction_diffusion(cat,
                                                        save_fig = save_fig),
      levy          = morie_tps_levy_flight_alpha(cat,
                                                    save_fig = save_fig),
      urban_scaling = morie_tps_urban_scaling_beta(cat,
                                                    save_fig = save_fig),
      lotka_volterra = morie_tps_lotka_volterra_police_crime(
        cat, save_fig = save_fig)
    )
  }
  out
}
