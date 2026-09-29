# SPDX-License-Identifier: AGPL-3.0-or-later
# Atmospheric dispersion models.
# Identical to the Python arm morie.fn.airdisp.

#' Atmospheric dispersion: Pasquill-Gifford sigmas, Briggs plume rise, Gaussian plume and puff, Eulerian and Lagrangian models
#'
#' \code{PgSigmas}: Briggs (1973) fits of the Pasquill-Gifford coefficients,
#' rural \code{sigma_y = a_y x (1 + 0.0001 x)^(-1/2)} (urban 0.0004) and
#' \code{sigma_z = a_z x (1 + b_z x)^(e_z)} by stability class A-F.
#' \code{BriggsPlumeRise}: buoyancy flux
#' \code{F = g v_s d^2 (T_s - T_a) / (4 T_s)}; final rise
#' \code{21.425 F^(3/4) / u} (\code{F < 55}, \code{x_f = 49 F^(5/8)}) or
#' \code{38.71 F^(3/5) / u} (\code{x_f = 119 F^(2/5)}) for classes A-D,
#' \code{2.6 (F / (u s))^(1/3)} with \code{s = g (dtheta/dz) / T_a} and
#' \code{x_f = 2.0715 u / sqrt(s)} for E and F; transitional rise
#' \code{1.6 F^(1/3) x^(2/3) / u} before \code{x_f} (EPA ISC3).
#' \code{GaussianPlume}: \code{Q / (2 pi u sigma_y sigma_z) exp(-y^2 / (2 sigma_y^2)) V}
#' with ground reflection and, under a mixing lid, image terms.
#' \code{GaussianPuff}: instantaneous puff with \code{sigma_x = sigma_y} at the
#' travel distance \code{u t}. \code{AdvectionDiffusion2d}: explicit upwind
#' advection, central diffusion, forward Euler, zero boundaries.
#' \code{LagrangianParticles}: random walk
#' \code{x + u dt + sqrt(2 K dt) xi} with Philox normals (step \code{k} uses
#' streams \code{2k} and \code{2k + 1}) and an optional counting grid.
#'
#' @param x Downwind distance(s) (m).
#' @param stability Pasquill stability class, "A" to "F".
#' @param setting "rural" or "urban".
#' @param u Wind speed (m/s), or the x velocity.
#' @param diameter,exit_velocity,stack_temp,ambient_temp Stack diameter (m), exit velocity (m/s),
#'   stack gas and ambient temperatures (K).
#' @param dtheta_dz Potential temperature gradient for stable classes (K/m).
#' @param g Gravity.
#' @param q Emission rate.
#' @param h Effective stack height.
#' @param receptors Matrix (or list) of receptor coordinates x, y, z.
#' @param mixing_height Mixing-lid height, or NULL for none.
#' @param n_images Number of lid image pairs.
#' @param mass Puff mass.
#' @param t Time since release.
#' @param c0 Initial concentration grid.
#' @param v Velocity along y.
#' @param kx,ky Eddy diffusivities.
#' @param dx,dy Grid spacings.
#' @param dt Time step.
#' @param n_steps Number of steps.
#' @param source Source grid (per unit time), or NULL.
#' @param n Number of particles.
#' @param x0,y0 Release point.
#' @param seed Philox seed.
#' @param grid NULL or c(x_min, x_max, y_min, y_max, nx, ny).
#' @return A list, or a numeric vector of concentrations.
#' @references Briggs, G. A. (1973). Diffusion estimation for small emissions.
#'   ATDL Contribution 79. Briggs, G. A. (1975). Plume rise predictions. AMS.
#'   Turner, D. B. (1970). Workbook of Atmospheric Dispersion Estimates. EPA
#'   AP-26. Seinfeld, J. H. and Pandis, S. N. (2016). Atmospheric Chemistry and
#'   Physics, 3rd ed. Thomson, D. J. (1987). J. Fluid Mech. 180, 529-556.
#' @examples
#' PgSigmas(1000, "D")
#' GaussianPlume(100, 5, 50, rbind(c(1000, 0, 0)))
#' @export
PgSigmas <- function(x, stability = "D", setting = "rural") {
  rural <- rbind(A = c(0.22, 0.20, 0, 0), B = c(0.16, 0.12, 0, 0), C = c(0.11, 0.08, 0.0002, -0.5),
                 D = c(0.08, 0.06, 0.0015, -0.5), E = c(0.06, 0.03, 0.0003, -1), F = c(0.04, 0.016, 0.0003, -1))
  urban <- rbind(A = c(0.32, 0.24, 0.001, 0.5), B = c(0.32, 0.24, 0.001, 0.5), C = c(0.22, 0.20, 0, 0),
                 D = c(0.16, 0.14, 0.0003, -0.5), E = c(0.11, 0.08, 0.0015, -0.5), F = c(0.11, 0.08, 0.0015, -0.5))
  co <- if (setting == "rural") rural[toupper(stability), ] else urban[toupper(stability), ]
  cy <- if (setting == "rural") 0.0001 else 0.0004
  x <- as.numeric(x)
  list(sigma_y = co[1] * x * (1 + cy * x)^-0.5, sigma_z = co[2] * x * (1 + co[3] * x)^co[4])
}

#' @rdname PgSigmas
#' @export
BriggsPlumeRise <- function(x, u, diameter, exit_velocity, stack_temp, ambient_temp, stability = "D",
                            dtheta_dz = NULL, g = 9.80616) {
  F_ <- g * exit_velocity * diameter^2 * (stack_temp - ambient_temp) / (4 * stack_temp)
  cls <- toupper(stability)
  if (cls %in% c("E", "F")) {
    lapse <- if (!is.null(dtheta_dz)) dtheta_dz else if (cls == "E") 0.020 else 0.035
    s <- g * lapse / ambient_temp
    final <- 2.6 * (F_ / (u * s))^(1 / 3)
    xf <- 2.0715 * u / sqrt(s)
  } else if (F_ < 55) {
    final <- 21.425 * F_^0.75 / u
    xf <- 49 * F_^(5 / 8)
  } else {
    final <- 38.71 * F_^0.6 / u
    xf <- 119 * F_^0.4
  }
  x <- as.numeric(x)
  rise <- ifelse(x >= xf, final, pmin(1.6 * F_^(1 / 3) * x^(2 / 3) / u, final))
  list(flux = F_, final_rise = final, x_final = xf, rise = rise)
}

.ad_vertical <- function(z, h, sz, lid, n_images) {
  tt <- exp(-(z - h)^2 / (2 * sz * sz)) + exp(-(z + h)^2 / (2 * sz * sz))
  if (!is.null(lid)) for (j in seq_len(n_images)) for (sgn in c(1, -1)) {
    off <- 2 * sgn * j * lid
    tt <- tt + exp(-(z - h + off)^2 / (2 * sz * sz)) + exp(-(z + h + off)^2 / (2 * sz * sz))
  }
  tt
}

.ad_rec <- function(r) if (is.list(r)) do.call(rbind, lapply(r, as.numeric)) else matrix(as.numeric(r), ncol = 3)

#' @rdname PgSigmas
#' @export
GaussianPlume <- function(q, u, h, receptors, stability = "D", setting = "rural", mixing_height = NULL,
                          n_images = 3) {
  R <- .ad_rec(receptors)
  vapply(seq_len(nrow(R)), function(i) {
    x <- R[i, 1]
    if (x <= 0) return(0)
    s <- PgSigmas(x, stability, setting)
    sy <- s$sigma_y
    sz <- s$sigma_z
    q / (2 * pi * u * sy * sz) * exp(-(R[i, 2]^2) / (2 * sy * sy)) * .ad_vertical(R[i, 3], h, sz, mixing_height, n_images)
  }, 0)
}

#' @rdname PgSigmas
#' @export
GaussianPuff <- function(mass, u, h, receptors, t, stability = "D", setting = "rural", mixing_height = NULL,
                         n_images = 3) {
  R <- .ad_rec(receptors)
  s <- PgSigmas(u * t, stability, setting)
  sy <- s$sigma_y
  sz <- s$sigma_z
  k <- mass / ((2 * pi)^1.5 * sy * sy * sz)
  vapply(seq_len(nrow(R)), function(i) {
    k * exp(-(R[i, 1] - u * t)^2 / (2 * sy * sy)) * exp(-(R[i, 2]^2) / (2 * sy * sy)) *
      .ad_vertical(R[i, 3], h, sz, mixing_height, n_images)
  }, 0)
}

#' @rdname PgSigmas
#' @export
AdvectionDiffusion2d <- function(c0, u, v, kx, ky, dx, dy, dt, n_steps, source = NULL) {
  cm <- unname(as.matrix(c0)) * 1
  nx <- nrow(cm)
  ny <- ncol(cm)
  ax <- u * dt / dx
  ay <- v * dt / dy
  dxn <- kx * dt / (dx * dx)
  dyn <- ky * dt / (dy * dy)
  for (step in seq_len(n_steps)) {
    nw <- matrix(0, nx, ny)
    for (i in seq_len(max(nx - 2, 0)) + 1) for (j in seq_len(max(ny - 2, 0)) + 1) {
      adv_x <- if (u >= 0) ax * (cm[i, j] - cm[i - 1, j]) else ax * (cm[i + 1, j] - cm[i, j])
      adv_y <- if (v >= 0) ay * (cm[i, j] - cm[i, j - 1]) else ay * (cm[i, j + 1] - cm[i, j])
      dif <- dxn * (cm[i + 1, j] - 2 * cm[i, j] + cm[i - 1, j]) + dyn * (cm[i, j + 1] - 2 * cm[i, j] + cm[i, j - 1])
      s <- if (!is.null(source)) source[i, j] * dt else 0
      nw[i, j] <- cm[i, j] - adv_x - adv_y + dif + s
    }
    cm <- nw
  }
  list(field = cm, mass = sum(cm) * dx * dy, cfl = abs(ax) + abs(ay), diffusion_number = 2 * (dxn + dyn))
}

#' @rdname PgSigmas
#' @export
LagrangianParticles <- function(n, x0, y0, u, v, kx, ky, dt, n_steps, seed = 0, grid = NULL) {
  xs <- rep(as.numeric(x0), n)
  ys <- rep(as.numeric(y0), n)
  fx <- sqrt(2 * kx * dt)
  fy <- sqrt(2 * ky * dt)
  for (k in seq_len(n_steps) - 1) {
    xs <- xs + u * dt + fx * .morie_random_normal(n, seed = seed, stream = 2 * k)
    ys <- ys + v * dt + fy * .morie_random_normal(n, seed = seed, stream = 2 * k + 1)
  }
  out <- list(x = xs, y = ys, mean_x = sum(xs) / n, mean_y = sum(ys) / n)
  if (!is.null(grid)) {
    wx <- (grid[2] - grid[1]) / grid[5]
    wy <- (grid[4] - grid[3]) / grid[6]
    conc <- matrix(0, grid[5], grid[6])
    ii <- floor((xs - grid[1]) / wx)
    jj <- floor((ys - grid[3]) / wy)
    for (p in seq_len(n)) if (ii[p] >= 0 && ii[p] < grid[5] && jj[p] >= 0 && jj[p] < grid[6]) {
      conc[ii[p] + 1, jj[p] + 1] <- conc[ii[p] + 1, jj[p] + 1] + 1 / (n * wx * wy)
    }
    out$concentration <- conc
  }
  out
}
