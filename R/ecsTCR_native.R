# R arm of ecsTCR -- equilibrium and transient climate sensitivity from a
# two-layer energy balance model.
#
# Equilibrium climate sensitivity is the warming once everything has
# settled after CO2 doubles; transient climate response is the warming at
# the moment of doubling in a run raising CO2 by 1% a year, which reaches
# doubling at year 70. ECS is a property of the balance, TCR of the
# balance and the ocean's heat uptake together, so TCR is the smaller.
#
#   C  dT/dt   = F(t) - lambda T - epsilon gamma (T - T_D)
#   C_D dT_D/dt =                        gamma (T - T_D)
#
# Routes, because a study has whichever it has:
#   parameters  lambda known, so ECS = F_2x / lambda exactly and TCR
#               comes from integrating the 1%/yr run.
#   gregory     Only a step experiment: regress the net top-of-atmosphere
#               imbalance on surface temperature, intercept is the
#               forcing and minus the slope is lambda.
#   emulate     Both a step run and a 1%/yr run are supplied.
#
# Solvers, since the choice shows in the third decimal of TCR:
#   analytic    diagonalise the 2x2 system; exact for constant forcing
#   rk4         classical fourth-order Runge-Kutta
#   euler       forward Euler, kept because it is what a quick
#               calculation uses and it is useful to see how wrong it is
#
# Defaults are AR6: F_2xCO2 = 3.93 W m-2. Charney's 1.5 to 4.5 K range is
# reported alongside so a result can be placed against the assessment
# that started the series.
#
# References
#   Charney, J.G. et al. (1979) "Carbon Dioxide and Climate: A Scientific
#     Assessment." National Academy of Sciences, Washington DC.
#   Forster, P. et al. (2021) "The Earth's Energy Budget, Climate
#     Feedbacks, and Climate Sensitivity", Chapter 7 of IPCC AR6 WG1,
#     Cambridge University Press, 923-1054,
#     doi:10.1017/9781009157896.009
#   Gregory, J.M. et al. (2004) "A new method for diagnosing radiative
#     forcing and climate sensitivity", Geophysical Research Letters
#     31(3), L03205, doi:10.1029/2003GL018747
#   Held, I.M. et al. (2010) "Probing the fast and slow components of
#     global warming by returning abruptly to preindustrial forcing",
#     Journal of Climate 23(9), 2418-2427, doi:10.1175/2009JCLI3466.1
#   Geoffroy, O. et al. (2013) "Transient climate response in a two-layer
#     energy-balance model. Part I", Journal of Climate 26(6), 1841-1857,
#     doi:10.1175/JCLI-D-12-00195.1

.ECSTCR_ROUTES <- c("parameters", "gregory", "emulate")
.ECSTCR_SOLVERS <- c("analytic", "rk4", "euler")

# AR6 Chapter 7: effective radiative forcing from doubling CO2.
.ECSTCR_F2X <- 3.93
# Charney (1979), the range that has anchored the question since.
.ECSTCR_CHARNEY <- c(1.5, 4.5)

# Compensated accumulation, so both language arms agree bit for bit.
# Neither language's sum() is a plain double loop -- R accumulates in
# long double, CPython 3.12 and later compensate -- and they are not
# unfaithful in the same way.
#' Compensated accumulation, so both language arms agree bit for bit
#'
#' Neither language\'s sum() is a plain double loop -- R accumulates in
#' long double, CPython 3.12 and later compensate -- and they are not
#' unfaithful in the same way.
#'
#' @param v A vector; its length is taken and its elements indexed.
#' @return A numeric value.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .ecstcr_csum(v = x)
#' res
.ecstcr_csum <- function(v) {
  s <- 0
  cc <- 0
  for (i in seq_along(v)) {
    t <- v[i]
    u <- s + t
    if (abs(s) >= abs(t)) cc <- cc + ((s - u) + t) else cc <- cc + ((t - u) + s)
    s <- u
  }
  s + cc
}

#' .ecstcr_mean
#'
#' A step of the ecsTCR_native implementation. Called by \code{.ecstcr_ols}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v A vector; its length is taken.
#' @return One of two values, depending on the branch taken.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .ecstcr_mean(v = x)
#' res
.ecstcr_mean <- function(v)
  if (length(v)) .ecstcr_csum(v) / length(v) else NA_real_

#' .ecstcr_deriv
#'
#' A step of the ecsTCR_native implementation. Called by \code{.ecstcr_euler}, \code{.ecstcr_rk4}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param T Numeric; combined arithmetically in the body.
#' @param TD Numeric; combined arithmetically in the body.
#' @param F Numeric; combined arithmetically in the body.
#' @param lam Numeric; combined arithmetically in the body.
#' @param gam Numeric; combined arithmetically in the body.
#' @param eps Numeric; combined arithmetically in the body.
#' @param C Numeric; combined arithmetically in the body.
#' @param CD Numeric; combined arithmetically in the body.
#' @return A vector, from \code{c}.
#' @export
.ecstcr_deriv <- function(T, TD, F, lam, gam, eps, C, CD)
  c((F - lam * T - eps * gam * (T - TD)) / C, gam * (T - TD) / CD)

#' .ecstcr_rk4
#'
#' A step of the ecsTCR_native implementation. Called by \code{.ecstcr_analytic}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param T Numeric; combined arithmetically in the body.
#' @param TD Numeric; combined arithmetically in the body.
#' @param F Passed to \code{.ecstcr_deriv}.
#' @param lam Passed to \code{.ecstcr_deriv}.
#' @param gam Passed to \code{.ecstcr_deriv}.
#' @param eps Passed to \code{.ecstcr_deriv}.
#' @param C Passed to \code{.ecstcr_deriv}.
#' @param CD Passed to \code{.ecstcr_deriv}.
#' @param h Numeric; combined arithmetically in the body.
#' @return A vector, from \code{c}.
#' @export
.ecstcr_rk4 <- function(T, TD, F, lam, gam, eps, C, CD, h) {
  k1 <- .ecstcr_deriv(T, TD, F, lam, gam, eps, C, CD)
  k2 <- .ecstcr_deriv(T + 0.5 * h * k1[1], TD + 0.5 * h * k1[2], F, lam,
                      gam, eps, C, CD)
  k3 <- .ecstcr_deriv(T + 0.5 * h * k2[1], TD + 0.5 * h * k2[2], F, lam,
                      gam, eps, C, CD)
  k4 <- .ecstcr_deriv(T + h * k3[1], TD + h * k3[2], F, lam, gam, eps,
                      C, CD)
  # The four-term combination goes through the compensated sum for the
  # same reason the dot products do: it is the one accumulation left in
  # the step, and a single differing bit here compounds over the run.
  c(T + h * .ecstcr_csum(c(k1[1], 2 * k2[1], 2 * k3[1], k4[1])) / 6,
    TD + h * .ecstcr_csum(c(k1[2], 2 * k2[2], 2 * k3[2], k4[2])) / 6)
}

#' .ecstcr_euler
#'
#' A step of the ecsTCR_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param T Numeric; combined arithmetically in the body.
#' @param TD Numeric; combined arithmetically in the body.
#' @param F Passed to \code{.ecstcr_deriv}.
#' @param lam Passed to \code{.ecstcr_deriv}.
#' @param gam Passed to \code{.ecstcr_deriv}.
#' @param eps Passed to \code{.ecstcr_deriv}.
#' @param C Passed to \code{.ecstcr_deriv}.
#' @param CD Passed to \code{.ecstcr_deriv}.
#' @param h Numeric; combined arithmetically in the body.
#' @return A vector, from \code{c}.
#' @export
.ecstcr_euler <- function(T, TD, F, lam, gam, eps, C, CD, h) {
  d <- .ecstcr_deriv(T, TD, F, lam, gam, eps, C, CD)
  c(T + h * d[1], TD + h * d[2])
}

# One step of the exact solution for a forcing held constant over the
# step. The system is linear, so diagonalising the 2x2 matrix gives the
# answer outright: no tolerance, no step-size sensitivity.
#' One step of the exact solution for a forcing held constant over the
#'
#' step. The system is linear, so diagonalising the 2x2 matrix gives the
#' answer outright: no tolerance, no step-size sensitivity.
#'
#' @param T Numeric; combined arithmetically in the body.
#' @param TD Numeric; combined arithmetically in the body.
#' @param F Numeric; combined arithmetically in the body.
#' @param lam Numeric; combined arithmetically in the body.
#' @param gam Numeric; combined arithmetically in the body.
#' @param eps Numeric; combined arithmetically in the body.
#' @param C Numeric; combined arithmetically in the body.
#' @param CD Numeric; combined arithmetically in the body.
#' @param h Numeric; combined arithmetically in the body.
#' @return A vector, from \code{c}.
#' @export
.ecstcr_analytic <- function(T, TD, F, lam, gam, eps, C, CD, h) {
  if (gam == 0) {
    # The one-layer model is not a degenerate case to be nursed through
    # the 2x2 machinery, it is a closed form: the deep ocean decouples
    # and the surface relaxes exponentially towards F/lambda. Going
    # through the eigen-decomposition divides by a zero determinant and
    # falls back to Runge-Kutta, costing six digits against a solution
    # that is known exactly.
    eq <- F / lam
    return(c(eq + (T - eq) * exp(-lam * h / C), TD))
  }
  a11 <- -(lam + eps * gam) / C
  a12 <- eps * gam / C
  a21 <- gam / CD
  a22 <- -gam / CD
  tr <- a11 + a22
  det <- a11 * a22 - a12 * a21
  disc <- tr * tr - 4 * det
  b1 <- F / C
  if (abs(det) < 1e-300 || disc < 0)
    return(.ecstcr_rk4(T, TD, F, lam, gam, eps, C, CD, h))
  sd <- sqrt(disc)
  r1 <- 0.5 * (tr + sd)
  r2 <- 0.5 * (tr - sd)
  eq1 <- -(a22 * b1) / det
  eq2 <- -(-a21 * b1) / det
  d1 <- T - eq1
  d2 <- TD - eq2
  if (abs(r1 - r2) < 1e-14)
    return(.ecstcr_rk4(T, TD, F, lam, gam, eps, C, CD, h))
  if (abs(a12) > 1e-300) {
    v1a <- a12
    v1b <- r1 - a11
    v2a <- a12
    v2b <- r2 - a11
  } else {
    v1a <- r1 - a22
    v1b <- a21
    v2a <- r2 - a22
    v2b <- a21
  }
  dd <- v1a * v2b - v2a * v1b
  if (abs(dd) < 1e-300)
    return(.ecstcr_rk4(T, TD, F, lam, gam, eps, C, CD, h))
  c1 <- (d1 * v2b - d2 * v2a) / dd
  c2 <- (d2 * v1a - d1 * v1b) / dd
  e1 <- exp(r1 * h)
  e2 <- exp(r2 * h)
  c(eq1 + c1 * v1a * e1 + c2 * v2a * e2,
    eq2 + c1 * v1b * e1 + c2 * v2b * e2)
}

#' Run the two-layer energy balance model
#'
#' @param forcing radiative forcing, one entry per year.
#' @param lam climate feedback parameter, W m-2 K-1.
#' @param gamma deep-ocean exchange coefficient.
#' @param epsilon efficacy of deep-ocean heat uptake.
#' @param C surface heat capacity, W yr m-2 K-1.
#' @param C_deep deep-ocean heat capacity.
#' @param solver analytic, rk4 or euler.
#' @param dt step length in years.
#' @param T0 initial surface anomaly.
#' @param TD0 initial deep anomaly.
#' @return a list with temperature, deep_temperature and imbalance.
#' @export
morie_ecsTCR_integrate <- function(forcing, lam, gamma = 0.7,
                                   epsilon = 1, C = 8, C_deep = 100,
                                   solver = "analytic", dt = 1, T0 = 0,
                                   TD0 = 0) {
  if (!(length(solver) == 1L && solver %in% .ECSTCR_SOLVERS))
    stop(sprintf("ecsTCR: solver = %s; expected one of %s", solver,
                 paste(.ECSTCR_SOLVERS, collapse = ", ")), call. = FALSE)
  step <- switch(solver, analytic = .ecstcr_analytic, rk4 = .ecstcr_rk4,
                 euler = .ecstcr_euler)
  T <- as.numeric(T0)
  TD <- as.numeric(TD0)
  n <- length(forcing)
  Ts <- numeric(n + 1L)
  TDs <- numeric(n + 1L)
  N <- numeric(n)
  Ts[1] <- T
  TDs[1] <- TD
  for (i in seq_len(n)) {
    F <- as.numeric(forcing[i])
    N[i] <- F - lam * T - (epsilon - 1) * gamma * (T - TD)
    st <- step(T, TD, F, lam, gamma, epsilon, C, C_deep, dt)
    T <- st[1]
    TD <- st[2]
    Ts[i + 1L] <- T
    TDs[i + 1L] <- TD
  }
  list(temperature = Ts, deep_temperature = TDs, imbalance = N)
}

#' Radiative forcing from a CO2 concentration ratio
#'
#' @param ratio CO2 relative to pre-industrial.
#' @param f2x forcing from doubling; defaults to AR6's 3.93 W m-2.
#' @return the forcing in W m-2.
#' @export
# The grouping matters. Written as f2x * log(r) / log(2) the division
# happens after the multiplication and a doubling comes back as
# 3.9299999999999997 rather than 3.93 -- close enough for climate, not
# close enough for a definition. Dividing the logs first gives exactly 1
# at a doubling and exactly 0 at no change.
morie_ecsTCR_co2_forcing <- function(ratio, f2x = .ECSTCR_F2X)
  f2x * (log(ratio) / log(2))

#' .ecstcr_ols
#'
#' A step of the ecsTCR_native implementation. Called by \code{morie_ecsTCR}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Numeric; combined arithmetically in the body.
#' @param y Numeric; combined arithmetically in the body.
#' @return A vector, from \code{c}.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' y <- c(2.9, 5.1, 6.8, 9.4, 11.2, 13.1, 15.0, 17.6)
#' res <- .ecstcr_ols(x = x, y = y)
#' res
.ecstcr_ols <- function(x, y) {
  mx <- .ecstcr_mean(x)
  my <- .ecstcr_mean(y)
  sxx <- .ecstcr_csum((x - mx) * (x - mx))
  sxy <- .ecstcr_csum((x - mx) * (y - my))
  if (sxx == 0)
    stop("ecsTCR: the temperature series has no spread, so the Gregory ",
         "regression is not identified", call. = FALSE)
  slope <- sxy / sxx
  c(slope, my - slope * mx)
}

#' Equilibrium and transient climate sensitivity
#'
#' @param model_run a surface temperature series; alias for temperature.
#' @param CO2_traj CO2 relative to pre-industrial, one entry per year.
#'   When absent the 1 percent per year trajectory is built from rate
#'   and years.
#' @param route parameters, gregory or emulate.
#' @param lam climate feedback parameter; required for the parameters
#'   route, fitted by the others.
#' @param gamma deep-ocean exchange coefficient.
#' @param epsilon efficacy of deep-ocean heat uptake.
#' @param C surface heat capacity.
#' @param C_deep deep-ocean heat capacity.
#' @param f2x forcing from doubling CO2; defaults to AR6's 3.93 W m-2.
#' @param solver analytic, rk4 or euler.
#' @param years length of the 1 percent per year run; 70 is the doubling
#'   year.
#' @param rate annual fractional CO2 increase.
#' @param temperature surface temperature series, for the fitting routes.
#' @param imbalance net top-of-atmosphere imbalance, for the Gregory
#'   route.
#' @param forcing_multiple what the step experiment did: 2 for
#'   abrupt-2x, 4 for abrupt-4x.
#' @param dt step length in years.
#' @return a list with ecs, tcr, tcr_ecs_ratio, lambda, f2x,
#'   doubling_year, realised_warming_fraction, temperature,
#'   deep_temperature, imbalance, fitted, charney_range, within_charney,
#'   route, solver and method.
#' @export
morie_ecsTCR <- function(model_run = NULL, CO2_traj = NULL,
                         route = "parameters", lam = NULL, gamma = 0.7,
                         epsilon = 1, C = 8, C_deep = 100,
                         f2x = .ECSTCR_F2X, solver = "analytic",
                         years = 70L, rate = 0.01, temperature = NULL,
                         imbalance = NULL, forcing_multiple = 2, dt = 1) {
  if (!(length(route) == 1L && route %in% .ECSTCR_ROUTES))
    stop(sprintf("ecsTCR: route = %s; expected one of %s", route,
                 paste(.ECSTCR_ROUTES, collapse = ", ")), call. = FALSE)
  if (is.null(temperature)) temperature <- model_run

  fitted <- NULL
  if (route %in% c("gregory", "emulate")) {
    if (is.null(temperature) || is.null(imbalance))
      stop(sprintf(paste("ecsTCR: the %s route needs both a temperature",
                         "series and the net imbalance"), route),
           call. = FALSE)
    Tv <- as.numeric(temperature)
    Nv <- as.numeric(imbalance)
    if (length(Tv) != length(Nv))
      stop(sprintf("ecsTCR: temperature has %d entries and imbalance %d",
                   length(Tv), length(Nv)), call. = FALSE)
    si <- .ecstcr_ols(Tv, Nv)
    lam_fit <- -si[1]
    if (lam_fit <= 0)
      stop(sprintf(paste("ecsTCR: the regression gives a non-positive",
                         "feedback parameter (%g), so the system has no",
                         "equilibrium"), lam_fit), call. = FALSE)
    scale <- log(forcing_multiple) / log(2)
    f2x <- si[2] / scale
    lam <- lam_fit
    fitted <- list(slope = si[1], intercept = si[2],
                   forcing_multiple = forcing_multiple)
  }

  if (is.null(lam))
    stop("ecsTCR: give lam, or use a route that fits it", call. = FALSE)
  if (lam <= 0)
    stop(sprintf(paste("ecsTCR: lam = %g; a non-positive feedback",
                       "parameter has no equilibrium"), lam), call. = FALSE)

  ecs <- f2x / lam

  # Built by repeated multiplication, not by raising to a power. R's ^
  # special-cases an integer exponent into repeated squaring while
  # Python's ** calls libm pow(), so the two disagree in the last bits
  # for the same nominal trajectory -- and the whole run is downstream
  # of it.
  traj <- if (is.null(CO2_traj)) {
    n <- as.integer(years)
    out <- numeric(n)
    acc <- 1
    for (i in seq_len(n)) { acc <- acc * (1 + rate)
    out[i] <- acc }
    out
  } else as.numeric(CO2_traj)
  forcing <- morie_ecsTCR_co2_forcing(traj, f2x)
  run <- morie_ecsTCR_integrate(forcing, lam, gamma, epsilon, C, C_deep,
                                solver = solver, dt = dt)

  # TCR is the warming at the moment of doubling: year 70 on the standard
  # trajectory, or the first year a supplied trajectory reaches twice
  # pre-industrial.
  idx <- length(traj)
  hit <- which(traj >= 2)
  if (length(hit)) idx <- hit[1]
  tcr <- run$temperature[idx + 1L]

  list(ecs = ecs, tcr = tcr,
       tcr_ecs_ratio = if (ecs != 0) tcr / ecs else NaN,
       lambda = lam, f2x = f2x, doubling_year = as.integer(idx),
       realised_warming_fraction = if (ecs != 0) tcr / ecs else NaN,
       temperature = run$temperature,
       deep_temperature = run$deep_temperature,
       imbalance = run$imbalance,
       fitted = fitted,
       charney_range = .ECSTCR_CHARNEY,
       within_charney = (ecs >= .ECSTCR_CHARNEY[1] &&
                         ecs <= .ECSTCR_CHARNEY[2]),
       route = route, solver = solver,
       method = sprintf(paste("two-layer energy balance (Held et al.",
                              "2010; Geoffroy et al. 2013), ECS = F_2x /",
                              "lambda, TCR at CO2 doubling in a %g%%/yr",
                              "run, %s route, %s solver"),
                        rate * 100, route, solver))
}
