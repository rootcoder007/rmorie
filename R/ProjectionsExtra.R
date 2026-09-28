#' Further map projections and geodesy
#'
#' \code{RobinsonProject}: Robinson projection with PROJ's coefficient
#' tables. \code{WebMercator}: EPSG:3857. \code{MapUnproject}: inverse
#' projections (webmerc, merc, sinu, moll, robin). \code{RotatedPole}:
#' geographic to rotated-pole coordinates and back (PROJ ob_tran, CF
#' rotated_latitude_longitude). \code{RotatedGrid}: true coordinates of a
#' regular rotated grid. \code{NormalGravity}: Somigliana normal gravity.
#' \code{GeoidHeight}: Bruns geoid undulation from fully normalised
#' coefficients. Identical to the Python arm \code{morie.fn.projextra}.
#'
#' @param lon,lat Degrees.
#' @param R Sphere radius.
#' @param lon_0 Central meridian (degrees).
#' @param x,y Projected coordinates (metres).
#' @param proj Projection name.
#' @param k_0 Scale factor.
#' @param pole_lon,pole_lat Rotated north pole in true coordinates.
#' @param inverse Map rotated coordinates back to true ones.
#' @param rlon,rlat Rotated grid axes (degrees).
#' @param C,S Lists of coefficient vectors by degree (index n + 1, order m + 1).
#' @param gm,a Gravitational constant times mass, and semi-major axis.
#' @return Numeric vector, list, or number.
#' @references Snyder, J. P. (1987). Map Projections: A Working Manual. USGS
#'   Professional Paper 1395.
#'
#'   Snyder, J. P. (1990). The Robinson projection: a computation algorithm.
#'   Cartography and Geographic Information Systems 17, 301-305.
#'
#'   Heiskanen, W. A. and Moritz, H. (1967). Physical Geodesy. Freeman.
#' @examples
#' RobinsonProject(10, 50)
#' MapUnproject(1113194.908, 6446275.841, "webmerc")
#' RotatedPole(10, 50, 10, 40)
#' @export
RobinsonProject <- function(lon, lat, R = 6378137, lon_0 = 0) {
  phi <- lat * pi / 180
  lam <- .prx_wrap((lon - lon_0) * pi / 180)
  d <- abs(phi)
  i <- min(floor(d * 11.45915590261646417544 + 1e-15), 18) + 1
  dd <- (d - 0.08726646259971647884 * (i - 1)) * 180 / pi
  x <- .prx_v(.prx_robx[i, ], dd) * 0.8487 * lam * R
  y <- .prx_v(.prx_roby[i, ], dd) * 1.3523 * R
  c(x, if (phi < 0) -y else y)
}

.prx_robx <- rbind(
  c(1.0, 2.21989997769713e-17, -7.155149796744809e-05, 3.1102999855647795e-06),
  c(0.9986000061035156, -0.0004822429909836501, -2.4896999093471095e-05, -1.3308999768923968e-06),
  c(0.9954000115394592, -0.0008310300181619823, -4.486049874685705e-05, -9.867010248854058e-07),
  c(0.9900000095367432, -0.0013536399928852916, -5.966100070509128e-05, 3.677700078696944e-06),
  c(0.982200026512146, -0.001674419967457652, -4.495469966059318e-06, -5.724109996663174e-06),
  c(0.9729999899864197, -0.0021486799232661724, -9.035709808813408e-05, 1.8735999418595384e-08),
  c(0.9599999785423279, -0.0030508500058203936, -9.007610060507432e-05, 1.6491700307597057e-06),
  c(0.9427000284194946, -0.003827919950708747, -6.533860141644254e-05, -2.6154000352107687e-06),
  c(0.9215999841690063, -0.004677460063248873, -0.00010456999734742567, 4.812429779121885e-06),
  c(0.8962000012397766, -0.005362229887396097, -3.2383100915467367e-05, -5.43431997357402e-06),
  c(0.867900013923645, -0.006093630101531744, -0.00011389800056349486, 3.324840008644969e-06),
  c(0.8349999785423279, -0.006983249913901091, -6.402529834304005e-05, 9.34959018650261e-07),
  c(0.7986000180244446, -0.007553379982709885, -5.000090095563792e-05, 9.353240102427662e-07),
  c(0.7597000002861023, -0.00798324029892683, -3.5970999306300655e-05, -2.276259920108714e-06),
  c(0.7185999751091003, -0.008513670414686203, -7.011489651631564e-05, -8.63029981701402e-06),
  c(0.6732000112533569, -0.009862090460956097, -0.00019956899632234126, 1.919739952427335e-05),
  c(0.6212999820709229, -0.0104179996997118, 8.839229849399999e-05, 6.240510174393421e-06),
  c(0.5722000002861023, -0.009066009894013405, 0.00018200000340584666, 6.240510174393421e-06),
  c(0.5321999788284302, -0.006777970120310783, 0.0002756080066319555, 6.240510174393421e-06)
)
.prx_roby <- rbind(
  c(-5.204170014340115e-18, 0.012400000356137753, 1.2143100314194296e-18, -8.452839816985858e-11),
  c(0.06199999898672104, 0.012400000356137753, -1.267929983228555e-09, 4.226420047270807e-10),
  c(0.12399999797344208, 0.012400000356137753, 5.071710162951604e-09, -1.6060399676831594e-09),
  c(0.1860000044107437, 0.012399899773299694, -1.9018900232481428e-08, 6.001520169718333e-09),
  c(0.24799999594688416, 0.01240019965916872, 7.100390320147199e-08, -2.240000007702747e-08),
  c(0.3100000023841858, 0.012399200350046158, -2.6499699856685766e-07, 8.359860004247821e-08),
  c(0.3720000088214874, 0.01240289956331253, 9.88982947092154e-07, -3.119940004125965e-07),
  c(0.4339999854564667, 0.012389300391077995, -3.6909300433762837e-06, -4.3562098994698317e-07),
  c(0.4957999885082245, 0.012319800443947315, -1.0225199730484746e-05, -3.455230057625158e-07),
  c(0.5570999979972839, 0.012191600166261196, -1.540810080769006e-05, -5.822880098094174e-07),
  c(0.6176000237464905, 0.011993800289928913, -2.4142400434357114e-05, -5.253269819149864e-07),
  c(0.6769000291824341, 0.011713000014424324, -3.202230072929524e-05, -5.164050094208505e-07),
  c(0.7346000075340271, 0.011354099959135056, -3.976840162067674e-05, -6.090519946155837e-07),
  c(0.7903000116348267, 0.01091070007532835, -4.8904199502430856e-05, -1.0473900147189852e-06),
  c(0.843500018119812, 0.010343099944293499, -6.461500015575439e-05, -1.4037400131172717e-09),
  c(0.8935999870300293, 0.009696859866380692, -6.463599856942892e-05, -8.54700010677334e-06),
  c(0.9394000172615051, 0.008409470319747925, -0.00019284100562799722, -4.210599854559405e-06),
  c(0.9761000275611877, 0.0061652702279388905, -0.00025599999935366213, -4.210599854559405e-06),
  c(1.0, 0.0032894699834287167, -0.0003191590076312423, -4.210599854559405e-06)
)

.prx_a <- 6378137
.prx_e2 <- (1 / 298.257223563) * (2 - 1 / 298.257223563)
.prx_e <- sqrt(.prx_e2)
.prx_v <- function(cc, z) cc[1] + z * (cc[2] + z * (cc[3] + z * cc[4]))
.prx_dv <- function(cc, z) cc[2] + 2 * z * cc[3] + z * z * 3 * cc[4]
.prx_wrap <- function(dl) (dl + pi) %% (2 * pi) - pi

#' @rdname RobinsonProject
#' @export
WebMercator <- function(lon, lat) c(.prx_a * lon * pi / 180, .prx_a * log(tan(pi / 4 + lat * pi / 360)))

#' @rdname RobinsonProject
#' @export
MapUnproject <- function(x, y, proj, R = NULL, lon_0 = 0, k_0 = 1) {
  l0 <- lon_0 * pi / 180
  if (proj == "webmerc") {
    lam <- x / .prx_a
    phi <- 2 * atan(exp(y / .prx_a)) - pi / 2
  } else if (proj == "merc") {
    t <- exp(-y / (.prx_a * k_0))
    phi <- pi / 2 - 2 * atan(t)
    for (it in 1:100) {
      s <- .prx_e * sin(phi)
      new <- pi / 2 - 2 * atan(t * ((1 - s) / (1 + s))^(.prx_e / 2))
      done <- abs(new - phi) < 1e-15
      phi <- new
      if (done) break
    }
    lam <- x / (.prx_a * k_0)
  } else if (proj == "sinu") {
    Rr <- if (is.null(R)) 6371000 else R
    phi <- y / Rr
    lam <- x / (Rr * cos(phi))
  } else if (proj == "moll") {
    Rr <- if (is.null(R)) 6371000 else R
    th <- asin(y / (sqrt(2) * Rr))
    phi <- asin((2 * th + sin(2 * th)) / pi)
    lam <- pi * x / (2 * sqrt(2) * Rr * cos(th))
  } else if (proj == "robin") {
    Rr <- if (is.null(R)) .prx_a else R
    lam <- x / Rr / 0.8487
    p <- abs(y / Rr / 1.3523)
    if (p >= 1) {
      phi <- sign(y) * pi / 2
      lam <- lam / .prx_robx[19, 1]
    } else {
      i <- floor(p * 18) + 1
      repeat {
        if (.prx_roby[i, 1] > p) {
          i <- i - 1
        } else if (.prx_roby[i + 1, 1] <= p) {
          i <- i + 1
        } else {
          break
        }
      }
      Tc <- .prx_roby[i, ]
      t <- 5 * (p - Tc[1]) / (.prx_roby[i + 1, 1] - Tc[1])
      for (it in 1:100) {
        t1 <- (.prx_v(Tc, t) - p) / .prx_dv(Tc, t)
        t <- t - t1
        if (abs(t1) < 1e-10) break
      }
      phi <- (5 * (i - 1) + t) * pi / 180
      if (y < 0) phi <- -phi
      lam <- lam / .prx_v(.prx_robx[i, ], t)
    }
  } else {
    stop("proj must be webmerc, merc, sinu, moll or robin")
  }
  c(.prx_wrap(lam + l0) * 180 / pi, phi * 180 / pi)
}

#' @rdname RobinsonProject
#' @export
RotatedPole <- function(lon, lat, pole_lon, pole_lat, inverse = FALSE) {
  pp <- pole_lat * pi / 180
  sp <- sin(pp)
  cp <- cos(pp)
  lon0 <- (180 + pole_lon) * pi / 180
  phi <- lat * pi / 180
  if (!inverse) {
    lam <- .prx_wrap(lon * pi / 180 - lon0)
    lo <- atan2(cos(phi) * sin(lam), sp * cos(phi) * cos(lam) + cp * sin(phi))
    la <- asin(max(-1, min(1, sp * sin(phi) - cp * cos(phi) * cos(lam))))
    return(c(.prx_wrap(lo) * 180 / pi, la * 180 / pi))
  }
  lam <- lon * pi / 180
  lo <- atan2(cos(phi) * sin(lam), sp * cos(phi) * cos(lam) - cp * sin(phi))
  la <- asin(max(-1, min(1, sp * sin(phi) + cp * cos(phi) * cos(lam))))
  c(.prx_wrap(lo + lon0) * 180 / pi, la * 180 / pi)
}

#' @rdname RobinsonProject
#' @export
RotatedGrid <- function(rlon, rlat, pole_lon, pole_lat) {
  lon <- matrix(0, length(rlat), length(rlon))
  lat <- lon
  for (i in seq_along(rlat)) {
    for (j in seq_along(rlon)) {
      v <- RotatedPole(rlon[j], rlat[i], pole_lon, pole_lat, inverse = TRUE)
      lon[i, j] <- v[1]
      lat[i, j] <- v[2]
    }
  }
  list(lon = lon, lat = lat)
}

#' @rdname RobinsonProject
#' @export
NormalGravity <- function(lat) {
  s2 <- sin(lat * pi / 180)^2
  9.7803253359 * (1 + 0.00193185265241 * s2) / sqrt(1 - .prx_e2 * s2)
}

#' @rdname RobinsonProject
#' @export
GeoidHeight <- function(lon, lat, C, S, gm = 3.986004418e14, a = 6378137) {
  phi <- lat * pi / 180
  lam <- lon * pi / 180
  t <- sin(phi)
  u <- cos(phi)
  nmax <- length(C) - 1
  P <- matrix(0, nmax + 1, nmax + 1)
  P[1, 1] <- 1
  if (nmax >= 1) {
    for (m in 1:nmax) P[m + 1, m + 1] <- (if (m == 1) sqrt(3) else sqrt((2 * m + 1) / (2 * m))) * u * P[m, m]
  }
  for (m in 0:nmax) {
    if (m + 1 <= nmax) P[m + 2, m + 1] <- sqrt(2 * m + 3) * t * P[m + 1, m + 1]
    if (m + 2 <= nmax) {
      for (n in (m + 2):nmax) {
        anm <- sqrt((2 * n - 1) * (2 * n + 1) / ((n - m) * (n + m)))
        bnm <- sqrt((2 * n + 1) * (n + m - 1) * (n - m - 1) / ((n - m) * (n + m) * (2 * n - 3)))
        P[n + 1, m + 1] <- anm * t * P[n, m + 1] - bnm * P[n - 1, m + 1]
      }
    }
  }
  s <- 0
  for (n in 0:nmax) {
    for (m in 0:min(n, length(C[[n + 1]]) - 1)) {
      s <- s + (C[[n + 1]][m + 1] * cos(m * lam) + S[[n + 1]][m + 1] * sin(m * lam)) * P[n + 1, m + 1]
    }
  }
  gm / (a * NormalGravity(lat)) * s
}
