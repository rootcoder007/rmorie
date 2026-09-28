.mar_ocx <- list(OC4 = c(0.3272, -2.9940, 2.7218, -1.2259, -0.5683), OC4v4 = c(0.366, -3.067, 1.930, 0.649, -1.532),
                 OC3M = c(0.2424, -2.7423, 1.8017, 0.0015, -1.2280), OC2 = c(0.2511, -2.0853, 1.5035, -3.1747, 0.3383))
.mar_k0 <- list(`mol/L` = c(-58.0931, 90.5069, 22.2940, 0.027766, -0.025888, 0.0050578),
                `mol/kg` = c(-60.2409, 93.4517, 23.3585, 0.023517, -0.023656, 0.0047036))

#' Marine remote sensing and ocean physics
#'
#' \code{OceanChlorophyll}: OCx maximum band-ratio chlorophyll.
#' \code{VgpmProduction}: Behrenfeld-Falkowski net primary production.
#' \code{EuphoticDepth}: 1 percent light depth from K_d or a Secchi depth.
#' \code{Co2Flux}: bulk air-sea CO2 flux (Wanninkhof 2014, Weiss 1974).
#' \code{OxygenSolubility}: Garcia-Gordon oxygen solubility.
#' \code{PracticalSalinity}: PSS-78 practical salinity.
#' \code{SplitWindowSst}: NLSST split-window temperature.
#' \code{SedimentTransport}: Shields number, critical Shields number and
#' Meyer-Peter-Mueller bedload. \code{OilSpillDrift}: Lagrangian drift with
#' Philox diffusion. \code{PlumeConcentration}: continuous point-source
#' plume. \code{DepthInvariantIndex}: Lyzenga bottom index.
#' \code{FloatingAlgaeIndex}, \code{MangroveVegetationIndex},
#' \code{BareSoilIndex}: spectral indices. Identical to the Python arm
#' \code{morie.fn.marine}.
#'
#' @param blue Blue reflectance(s): vector, or matrix with one row per pixel.
#' @param green Green reflectance.
#' @param algorithm \code{"OC4"}, \code{"OC4v4"}, \code{"OC3M"} or \code{"OC2"}.
#' @param coefficients Polynomial or split-window coefficients.
#' @param chl Chlorophyll (mg per cubic m).
#' @param par Daily PAR (mol photons per square m per day).
#' @param sst Sea-surface temperature (C).
#' @param day_length Day length (h).
#' @param kd Diffuse attenuation coefficient (1/m).
#' @param secchi Secchi depth (m).
#' @param fraction Light fraction at the euphotic depth.
#' @param u10 Wind speed at 10 m (m/s).
#' @param sss Salinity.
#' @param pco2_water,pco2_air pCO2 (microatm).
#' @param k0_units \code{"mol/L"} or \code{"mol/kg"}.
#' @param temperature Temperature (C).
#' @param salinity Practical salinity.
#' @param units \code{"umol/kg"} or \code{"ml/l"}.
#' @param conductivity Conductivity (mS/cm).
#' @param pressure Sea pressure (dbar).
#' @param t11,t12 Brightness temperatures.
#' @param sst_first_guess First-guess temperature (NULL uses t11).
#' @param zenith Satellite zenith angle (degrees).
#' @param tau Bed shear stress (Pa).
#' @param d Grain diameter (m).
#' @param rho_s,rho Sediment and water densities.
#' @param nu Kinematic viscosity.
#' @param g Gravity.
#' @param x0,y0 Release position (m).
#' @param current,wind Constant (u, v) velocities (m/s).
#' @param hours Duration (h).
#' @param dt Time step (h).
#' @param n_particles Number of particles.
#' @param wind_factor Wind drift fraction.
#' @param diffusivity Horizontal diffusivity (square m per s).
#' @param seed Philox seed.
#' @param x,y Receptor coordinates (m).
#' @param load Discharge (mass per s).
#' @param u Current speed.
#' @param depth Water depth.
#' @param ky Transverse diffusivity.
#' @param decay First-order decay rate.
#' @param band_i,band_j Band radiances or reflectances.
#' @param deep_i,deep_j Deep-water signals.
#' @param sand_i,sand_j Sand pixels at varying depth.
#' @param red,nir,swir,swir1 Reflectances.
#' @param wavelengths Red, NIR and SWIR wavelengths (nm).
#' @return A number, vector or list.
#' @references O'Reilly, J. E. et al. (1998). Ocean color chlorophyll
#'   algorithms for SeaWiFS. Journal of Geophysical Research 103, 24937-24953.
#'
#'   Behrenfeld, M. J. and Falkowski, P. G. (1997). Photosynthetic rates
#'   derived from satellite-based chlorophyll concentration. Limnology and
#'   Oceanography 42, 1-20.
#'
#'   Wanninkhof, R. (2014). Relationship between wind speed and gas exchange
#'   over the ocean revisited. Limnology and Oceanography: Methods 12,
#'   351-362.
#'
#'   Garcia, H. E. and Gordon, L. I. (1992). Oxygen solubility in seawater:
#'   better fitting equations. Limnology and Oceanography 37, 1307-1312.
#'
#'   UNESCO (1983). Algorithms for computation of fundamental properties of
#'   seawater. UNESCO Technical Papers in Marine Science 44.
#'
#'   Lyzenga, D. R. (1981). Remote sensing of bottom reflectance and water
#'   attenuation parameters in shallow water using aircraft and Landsat data.
#'   International Journal of Remote Sensing 2, 71-82.
#'
#'   Hu, C. (2009). A novel ocean color index to detect floating algae in the
#'   global oceans. Remote Sensing of Environment 113, 2118-2129.
#' @examples
#' OxygenSolubility(10, 35)
#' PracticalSalinity(42.914, 15 / 1.00024)
#' Co2Flux(7, 20, 35, 420, 400)$flux
#' @export
OceanChlorophyll <- function(blue, green, algorithm = "OC4", coefficients = NULL) {
  a <- if (is.null(coefficients)) .mar_ocx[[algorithm]] else coefficients
  B <- if (is.matrix(blue)) blue else matrix(blue, ncol = 1)
  x <- log10(apply(B, 1, max) / green)
  vapply(x, function(v) 10^sum(a * v^(seq_along(a) - 1)), numeric(1))
}

#' @rdname OceanChlorophyll
#' @export
EuphoticDepth <- function(kd = NULL, secchi = NULL, fraction = 0.01) {
  if (is.null(kd)) {
    if (is.null(secchi)) stop("give kd or secchi")
    kd <- 1.7 / secchi
  }
  -log(fraction) / kd
}

.mar_pbopt <- function(t) {
  cf <- c(1.2956, 2.749e-1, 6.17e-2, -2.05e-2, 2.462e-3, -1.348e-4, 3.4132e-6, -3.27e-8)
  vapply(t, function(v) {
    if (v < -10) 0 else if (v < -1) 1.13 else if (v > 28.5) 4 else sum(cf * v^(0:7))
  }, numeric(1))
}

#' @rdname OceanChlorophyll
#' @export
VgpmProduction <- function(chl, par, sst, day_length) {
  tot <- ifelse(chl < 1, 38 * chl^0.425, 40.2 * chl^0.507)
  z <- 200 * tot^-0.293
  z <- ifelse(z <= 102, 568.2 * tot^-0.746, z)
  .mar_pbopt(sst) * chl * day_length * 0.66125 * par / (par + 4.1) * z
}

#' @rdname OceanChlorophyll
#' @export
Co2Flux <- function(u10, sst, sss, pco2_water, pco2_air, k0_units = "mol/L") {
  sc <- 2116.8 - 136.25 * sst + 4.7353 * sst^2 - 0.092307 * sst^3 + 0.0007555 * sst^4
  k <- 0.251 * u10^2 * (sc / 660)^-0.5
  tk <- (sst + 273.15) / 100
  cf <- .mar_k0[[k0_units]]
  k0 <- exp(cf[1] + cf[2] / tk + cf[3] * log(tk) + sss * (cf[4] + cf[5] * tk + cf[6] * tk^2))
  list(flux = 0.24 * k * k0 * (pco2_water - pco2_air), transfer_velocity = k, schmidt = sc, k0 = k0)
}

#' @rdname OceanChlorophyll
#' @export
OxygenSolubility <- function(temperature, salinity, units = "umol/kg") {
  if (units == "umol/kg") {
    A <- c(5.80871, 3.20291, 4.17887, 5.10006, -9.86643e-2, 3.80369)
    B <- c(-7.01577e-3, -7.70028e-3, -1.13864e-2, -9.51519e-3)
    C0 <- -2.75915e-7
  } else if (units == "ml/l") {
    A <- c(2.00907, 3.22014, 4.05010, 4.94457, -2.56847e-1, 3.88767)
    B <- c(-6.24523e-3, -7.37614e-3, -1.03410e-2, -8.17083e-3)
    C0 <- -4.88682e-7
  } else {
    stop("units must be 'umol/kg' or 'ml/l'")
  }
  t68 <- 1.00024 * temperature
  ts <- log((298.15 - t68) / (273.15 + t68))
  exp(vapply(ts, function(v) sum(A * v^(0:5)), numeric(1)) +
        salinity * vapply(ts, function(v) sum(B * v^(0:3)), numeric(1)) + C0 * salinity^2)
}

#' @rdname OceanChlorophyll
#' @export
PracticalSalinity <- function(conductivity, temperature, pressure = 0) {
  a <- c(0.0080, -0.1692, 25.3851, 14.0941, -7.0261, 2.7081)
  b <- c(0.0005, -0.0056, -0.0066, -0.0375, 0.0636, -0.0144)
  cc <- c(0.6766097, 2.00564e-2, 1.104259e-4, -6.9698e-7, 1.0031e-9)
  n <- max(length(conductivity), length(temperature), length(pressure))
  cond <- rep_len(conductivity, n)
  t <- 1.00024 * rep_len(temperature, n)
  p <- rep_len(pressure, n)
  R <- cond / 42.914
  rt <- vapply(t, function(v) sum(cc * v^(0:4)), numeric(1))
  Rp <- 1 + p * (2.070e-5 + p * (-6.370e-10 + p * 3.989e-15)) / (1 + 3.426e-2 * t + 4.464e-4 * t^2 +
                                                                  (4.215e-1 - 3.107e-3 * t) * R)
  r <- sqrt(R / (Rp * rt))
  vapply(seq_len(n), function(i) sum(a * r[i]^(0:5)) + (t[i] - 15) / (1 + 0.0162 * (t[i] - 15)) *
           sum(b * r[i]^(0:5)), numeric(1))
}

#' @rdname OceanChlorophyll
#' @export
SplitWindowSst <- function(t11, t12, coefficients, sst_first_guess = NULL, zenith = 0) {
  gs <- if (is.null(sst_first_guess)) t11 else sst_first_guess
  coefficients[1] + coefficients[2] * t11 + coefficients[3] * (t11 - t12) * gs +
    coefficients[4] * (t11 - t12) * (1 / cos(zenith * pi / 180) - 1)
}

#' @rdname OceanChlorophyll
#' @export
SedimentTransport <- function(tau, d, rho_s = 2650, rho = 1025, nu = 1.36e-6, g = 9.81) {
  s <- rho_s / rho
  th <- tau / ((rho_s - rho) * g * d)
  ds <- d * ((s - 1) * g / nu^2)^(1 / 3)
  tc <- 0.30 / (1 + 1.2 * ds) + 0.055 * (1 - exp(-0.020 * ds))
  q <- ifelse(th > tc, 8 * pmax(th - tc, 0)^1.5 * sqrt((s - 1) * g * d^3), 0)
  list(theta = th, theta_cr = tc, d_star = ds, bedload = q)
}

#' @rdname OceanChlorophyll
#' @export
OilSpillDrift <- function(x0, y0, current, wind, hours, dt = 1, n_particles = 100, wind_factor = 0.03,
                          diffusivity = 10, seed = 0) {
  steps <- round(hours / dt)
  sec <- dt * 3600
  ux <- current[1] + wind_factor * wind[1]
  uy <- current[2] + wind_factor * wind[2]
  sd <- sqrt(2 * diffusivity * sec)
  z <- .morie_random_normal(2 * steps * n_particles, seed = seed)
  xs <- rep(x0, n_particles)
  ys <- rep(y0, n_particles)
  k <- 1
  for (s in seq_len(steps)) {
    for (p in seq_len(n_particles)) {
      xs[p] <- xs[p] + ux * sec + sd * z[k]
      ys[p] <- ys[p] + uy * sec + sd * z[k + 1]
      k <- k + 2
    }
  }
  list(x = xs, y = ys, centroid = c(sum(xs), sum(ys)) / n_particles)
}

#' @rdname OceanChlorophyll
#' @export
PlumeConcentration <- function(x, y, load, u, depth, ky, decay = 0) {
  out <- load / (depth * sqrt(4 * pi * ky * pmax(x, 1e-300) * u)) * exp(-u * y^2 / (4 * ky * pmax(x, 1e-300))) *
    exp(-decay * x / u)
  ifelse(x > 0, out, 0)
}

#' @rdname OceanChlorophyll
#' @export
DepthInvariantIndex <- function(band_i, band_j, deep_i, deep_j, sand_i, sand_j) {
  si <- log(sand_i - deep_i)
  sj <- log(sand_j - deep_j)
  n <- length(si)
  mi <- sum(si) / n
  mj <- sum(sj) / n
  a <- (sum((si - mi)^2) / (n - 1) - sum((sj - mj)^2) / (n - 1)) / (2 * sum((si - mi) * (sj - mj)) / (n - 1))
  ratio <- a + sqrt(a^2 + 1)
  list(dii = log(band_i - deep_i) - ratio * log(band_j - deep_j), ratio = ratio)
}

#' @rdname OceanChlorophyll
#' @export
FloatingAlgaeIndex <- function(red, nir, swir, wavelengths = c(645, 859, 1240)) {
  nir - (red + (swir - red) * (wavelengths[2] - wavelengths[1]) / (wavelengths[3] - wavelengths[1]))
}

#' @rdname OceanChlorophyll
#' @export
MangroveVegetationIndex <- function(green, nir, swir1) (nir - green) / (swir1 - green)

#' @rdname OceanChlorophyll
#' @export
BareSoilIndex <- function(blue, red, nir, swir) ((swir + red) - (nir + blue)) / ((swir + red) + (nir + blue))
