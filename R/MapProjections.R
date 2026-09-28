.mp_a <- 6378137
.mp_f <- 1 / 298.257223563
.mp_e2 <- .mp_f * (2 - .mp_f)
.mp_e <- sqrt(.mp_e2)

.mp_q <- function(phi) {
  s <- sin(phi)
  (1 - .mp_e2) * (s / (1 - .mp_e2 * s^2) - log((1 - .mp_e * s) / (1 + .mp_e * s)) / (2 * .mp_e))
}
.mp_m <- function(phi) cos(phi) / sqrt(1 - .mp_e2 * sin(phi)^2)
.mp_t <- function(phi) {
  s <- sin(phi)
  tan(pi / 4 - phi / 2) / ((1 - .mp_e * s) / (1 + .mp_e * s))^(.mp_e / 2)
}

.mp_newton <- function(f, df, x) {
  for (it in 1:100) {
    dx <- f(x) / df(x)
    x <- x - dx
    if (abs(dx) < 1e-15) break
  }
  x
}

.mp_tmerc <- function(lam, phi, lon0, k0, fe, fn) {
  n <- .mp_f / (2 - .mp_f)
  A <- .mp_a / (1 + n) * (1 + n^2 / 4 + n^4 / 64 + n^6 / 256)
  al <- c(n / 2 - 2 * n^2 / 3 + 5 * n^3 / 16 + 41 * n^4 / 180 - 127 * n^5 / 288 + 7891 * n^6 / 37800,
          13 * n^2 / 48 - 3 * n^3 / 5 + 557 * n^4 / 1440 + 281 * n^5 / 630 - 1983433 * n^6 / 1935360,
          61 * n^3 / 240 - 103 * n^4 / 140 + 15061 * n^5 / 26880 + 167603 * n^6 / 181440,
          49561 * n^4 / 161280 - 179 * n^5 / 168 + 6601661 * n^6 / 7257600,
          34729 * n^5 / 80640 - 3418889 * n^6 / 1995840,
          212378941 * n^6 / 319334400)
  dl <- lam - lon0
  t <- sinh(atanh(sin(phi)) - .mp_e * atanh(.mp_e * sin(phi)))
  xi <- atan2(t, cos(dl))
  eta <- atanh(sin(dl) / sqrt(1 + t^2))
  j <- 1:6
  c(fe + k0 * A * (eta + sum(al * cos(2 * j * xi) * sinh(2 * j * eta))),
    fn + k0 * A * (xi + sum(al * sin(2 * j * xi) * cosh(2 * j * eta))))
}

#' Map projections, UTM zones, local tangent planes and geodesics
#'
#' \code{MapProject}: forward projections of Snyder (1987) - spherical
#' sinu, moll, eck4, goode (PROJ constants), wintri (Winkel's standard
#' parallel acos(2/pi)), bonne, cass, aeqd, stere; ellipsoidal WGS84 merc,
#' lcc, aea, tmerc (Krueger series to order 6; Karney 2011) and utm - with
#' longitude differences wrapped as PROJ. \code{UtmZone}: zone with the
#' Norway and Svalbard exceptions. \code{GeodeticToEnu}: ECEF and local
#' east-north-up coordinates. \code{GreatCircleDistance} and
#' \code{VincentyInverse} (Vincenty 1975). Identical to the Python arm
#' \code{morie.fn.mapproj}.
#'
#' @param lon,lat Degrees.
#' @param proj Projection name.
#' @param R Sphere radius.
#' @param lon_0,lat_0 Projection centre.
#' @param lat_1,lat_2 Standard parallels.
#' @param k_0 Scale factor.
#' @param zone UTM zone.
#' @param south Southern hemisphere UTM.
#' @param h Ellipsoidal height.
#' @param lon0,lat0,h0 Local origin.
#' @param lon1,lat1,lon2,lat2 Endpoints.
#' @param r Radius for the haversine distance.
#' @param tol Convergence tolerance.
#' @param maxit Maximum iterations.
#' @return Coordinates, zone, distance or list.
#' @references Snyder, J. P. (1987). Map Projections: A Working Manual. U.S.
#'   Geological Survey Professional Paper 1395.
#'
#'   Karney, C. F. F. (2011). Transverse Mercator with an accuracy of a few
#'   nanometers. Journal of Geodesy 85, 475-485.
#'
#'   Vincenty, T. (1975). Direct and inverse solutions of geodesics on the
#'   ellipsoid with application of nested equations. Survey Review 23,
#'   88-93.
#' @examples
#' MapProject(10, 50, "sinu")
#' UtmZone(10, 50)
#' VincentyInverse(0, 0, 1, 0)$distance
#' @export
MapProject <- function(lon, lat, proj, R = 6371000, lon_0 = 0, lat_0 = 0, lat_1 = NULL, lat_2 = NULL, k_0 = 1,
                       zone = NULL, south = FALSE) {
  lam <- lon * pi / 180
  phi <- lat * pi / 180
  l0 <- lon_0 * pi / 180
  p0 <- lat_0 * pi / 180
  dl <- (lam - l0 + pi) %% (2 * pi) - pi
  newton_moll <- function() {
    if (abs(abs(phi) - pi / 2) < 1e-15) return(sign(phi) * pi / 2)
    .mp_newton(function(t) 2 * t + sin(2 * t) - pi * sin(phi), function(t) 2 + 2 * cos(2 * t), phi)
  }
  switch(proj,
    sinu = c(R * dl * cos(phi), R * phi),
    moll = , goode = {
      lim <- (40 + 44 / 60 + 11.8 / 3600) * pi / 180
      if (proj == "goode" && abs(phi) <= lim) return(c(R * dl * cos(phi), R * phi))
      th <- newton_moll()
      y <- sqrt(2) * R * sin(th)
      if (proj == "goode") y <- y - sign(phi) * 0.05280 * R
      c(2 * sqrt(2) / pi * R * dl * cos(th), y)
    },
    eck4 = {
      cc <- 2 + pi / 2
      th <- if (abs(abs(phi) - pi / 2) < 1e-15) sign(phi) * pi / 2 else {
        .mp_newton(function(t) t + sin(t) * cos(t) + 2 * sin(t) - cc * sin(phi),
                   function(t) 2 * cos(t) * (1 + cos(t)), phi / 2)
      }
      c(2 / sqrt(pi * (4 + pi)) * R * dl * (1 + cos(th)), 2 * sqrt(pi / (4 + pi)) * R * sin(th))
    },
    wintri = {
      p1 <- if (is.null(lat_1)) acos(2 / pi) else lat_1 * pi / 180
      a <- acos(cos(phi) * cos(dl / 2))
      sinc <- if (a != 0) sin(a) / a else 1
      c(0.5 * R * (dl * cos(p1) + 2 * cos(phi) * sin(dl / 2) / sinc), 0.5 * R * (phi + sin(phi) / sinc))
    },
    bonne = {
      p1 <- (if (is.null(lat_1)) 45 else lat_1) * pi / 180
      rho <- 1 / tan(p1) + p1 - phi
      E <- if (rho != 0) dl * cos(phi) / rho else 0
      c(R * rho * sin(E), R * (1 / tan(p1) - rho * cos(E)))
    },
    cass = c(R * asin(cos(phi) * sin(dl)), R * (atan2(tan(phi), cos(dl)) - p0)),
    aeqd = , stere = {
      cc <- sin(p0) * sin(phi) + cos(p0) * cos(phi) * cos(dl)
      k <- if (proj == "aeqd") {
        cz <- acos(max(-1, min(1, cc)))
        if (cz != 0) cz / sin(cz) else 1
      } else {
        2 * k_0 / (1 + cc)
      }
      c(R * k * cos(phi) * sin(dl), R * k * (cos(p0) * sin(phi) - sin(p0) * cos(phi) * cos(dl)))
    },
    merc = {
      s <- sin(phi)
      c(.mp_a * k_0 * dl, .mp_a * k_0 * log(tan(pi / 4 + phi / 2) * ((1 - .mp_e * s) / (1 + .mp_e * s))^(.mp_e / 2)))
    },
    lcc = {
      p1 <- lat_1 * pi / 180
      p2 <- (if (is.null(lat_2)) lat_1 else lat_2) * pi / 180
      n <- if (p1 != p2) (log(.mp_m(p1)) - log(.mp_m(p2))) / (log(.mp_t(p1)) - log(.mp_t(p2))) else sin(p1)
      Fc <- .mp_m(p1) / (n * .mp_t(p1)^n)
      rho <- .mp_a * Fc * .mp_t(phi)^n
      rho0 <- .mp_a * Fc * .mp_t(p0)^n
      c(rho * sin(n * dl), rho0 - rho * cos(n * dl))
    },
    aea = {
      p1 <- lat_1 * pi / 180
      p2 <- (if (is.null(lat_2)) lat_1 else lat_2) * pi / 180
      n <- if (p1 != p2) (.mp_m(p1)^2 - .mp_m(p2)^2) / (.mp_q(p2) - .mp_q(p1)) else sin(p1)
      C <- .mp_m(p1)^2 + n * .mp_q(p1)
      rho <- .mp_a * sqrt(C - n * .mp_q(phi)) / n
      rho0 <- .mp_a * sqrt(C - n * .mp_q(p0)) / n
      c(rho * sin(n * dl), rho0 - rho * cos(n * dl))
    },
    tmerc = .mp_tmerc(lam, phi, l0, k_0, 0, 0),
    utm = {
      z <- if (is.null(zone)) UtmZone(lon, lat) else zone
      .mp_tmerc(lam, phi, (-183 + 6 * z) * pi / 180, 0.9996, 5e5, if (south) 1e7 else 0)
    },
    stop("unknown projection")
  )
}

#' @rdname MapProject
#' @export
UtmZone <- function(lon, lat) {
  z <- floor((lon + 180) / 6) %% 60 + 1
  if (lat >= 56 && lat < 64 && lon >= 3 && lon < 12) return(32)
  if (lat >= 72 && lat < 84 && lon >= 0) {
    if (lon < 9) return(31)
    if (lon < 21) return(33)
    if (lon < 33) return(35)
    if (lon < 42) return(37)
  }
  z
}

.mp_ecef <- function(lon, lat, h) {
  lam <- lon * pi / 180
  phi <- lat * pi / 180
  N <- .mp_a / sqrt(1 - .mp_e2 * sin(phi)^2)
  c((N + h) * cos(phi) * cos(lam), (N + h) * cos(phi) * sin(lam), (N * (1 - .mp_e2) + h) * sin(phi))
}

#' @rdname MapProject
#' @export
GeodeticToEnu <- function(lon, lat, h, lon0, lat0, h0) {
  d <- .mp_ecef(lon, lat, h) - .mp_ecef(lon0, lat0, h0)
  l0 <- lon0 * pi / 180
  p0 <- lat0 * pi / 180
  Rm <- rbind(c(-sin(l0), cos(l0), 0), c(-sin(p0) * cos(l0), -sin(p0) * sin(l0), cos(p0)),
              c(cos(p0) * cos(l0), cos(p0) * sin(l0), sin(p0)))
  list(enu = as.vector(Rm %*% d), ecef = .mp_ecef(lon, lat, h))
}

#' @rdname MapProject
#' @export
GreatCircleDistance <- function(lon1, lat1, lon2, lat2, r = 6378137) {
  p1 <- lat1 * pi / 180
  p2 <- lat2 * pi / 180
  a <- sin((p2 - p1) / 2)^2 + cos(p1) * cos(p2) * sin((lon2 - lon1) * pi / 360)^2
  2 * r * atan2(sqrt(a), sqrt(1 - a))
}

#' @rdname MapProject
#' @export
VincentyInverse <- function(lon1, lat1, lon2, lat2, tol = 1e-13, maxit = 200L) {
  a <- .mp_a
  f <- .mp_f
  b <- a * (1 - f)
  L <- (lon2 - lon1) * pi / 180
  U1 <- atan((1 - f) * tan(lat1 * pi / 180))
  U2 <- atan((1 - f) * tan(lat2 * pi / 180))
  lam <- L
  conv <- FALSE
  for (it in seq_len(maxit)) {
    ss <- sqrt((cos(U2) * sin(lam))^2 + (cos(U1) * sin(U2) - sin(U1) * cos(U2) * cos(lam))^2)
    if (ss == 0) return(list(distance = 0, azimuth1 = 0, azimuth2 = 0))
    cs <- sin(U1) * sin(U2) + cos(U1) * cos(U2) * cos(lam)
    sig <- atan2(ss, cs)
    sa <- cos(U1) * cos(U2) * sin(lam) / ss
    c2a <- 1 - sa^2
    c2sm <- if (c2a != 0) cs - 2 * sin(U1) * sin(U2) / c2a else 0
    C <- f / 16 * c2a * (4 + f * (4 - 3 * c2a))
    lp <- lam
    lam <- L + (1 - C) * f * sa * (sig + C * ss * (c2sm + C * cs * (-1 + 2 * c2sm^2)))
    if (abs(lam - lp) < tol) {
      conv <- TRUE
      break
    }
  }
  if (!conv) return(list(distance = NaN, azimuth1 = NaN, azimuth2 = NaN))
  u2 <- c2a * (a^2 - b^2) / b^2
  A <- 1 + u2 / 16384 * (4096 + u2 * (-768 + u2 * (320 - 175 * u2)))
  B <- u2 / 1024 * (256 + u2 * (-128 + u2 * (74 - 47 * u2)))
  ds <- B * ss * (c2sm + B / 4 * (cs * (-1 + 2 * c2sm^2) - B / 6 * c2sm * (-3 + 4 * ss^2) * (-3 + 4 * c2sm^2)))
  az1 <- atan2(cos(U2) * sin(lam), cos(U1) * sin(U2) - sin(U1) * cos(U2) * cos(lam)) * 180 / pi
  az2 <- atan2(cos(U1) * sin(lam), -sin(U1) * cos(U2) + cos(U1) * sin(U2) * cos(lam)) * 180 / pi
  list(distance = b * A * (sig - ds), azimuth1 = az1 %% 360, azimuth2 = az2 %% 360)
}
