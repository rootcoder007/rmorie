# SPDX-License-Identifier: AGPL-3.0-or-later
#' Geomasking: spatial data distortion for privacy protection
#'
#' Displaces point locations by bounded random noise and reports how much
#' the spatial signal moved.  Each point is shifted to a uniformly random
#' location inside a disc of radius r centred on it.  Uniformity over the
#' disc requires the radial draw \code{rho = r sqrt(u1)} with
#' \code{theta = 2 pi u2} for independent uniforms u1, u2 -- taking
#' \code{rho = r u1} would over-concentrate points near the centre, which is
#' exactly the failure mode that makes a mask look stronger than it is.  The
#' displaced point is \code{(x + rho cos(theta), y + rho sin(theta))}.
#'
#' Under this mask \code{E\[rho\] = 2 r / 3} and \code{E\[rho^2\] = r^2 / 2}, so
#' both the mean and the RMS displacement have closed forms the sample
#' values can be checked against; they are returned as
#' \code{expected_displacement} and \code{expected_rms}.
#'
#' The mask preserves the mean centre in expectation but not exactly; the
#' realised shift of the mean centre is reported, as is the change in mean
#' pairwise distance, the quantity most spatial analyses actually depend on.
#' Allshouse et al. evaluate exactly this trade-off between
#' re-identification risk and analytic distortion.
#'
#' The uniform stream is the Lehmer minstd generator shared with every other
#' arm of this package, so a given seed reproduces the same mask in R and in
#' Python bit for bit.
#'
#' @param coords Point coordinates, n rows by 2 columns (x, y).
#' @param noise_radius Maximum displacement r, non-negative.
#' @param seed Seed for the shared minstd stream. Default 1.
#' @return List with \code{estimate} (mean realised displacement),
#'   \code{masked}, \code{displacement}, \code{mean_displacement},
#'   \code{max_displacement}, \code{rms_displacement},
#'   \code{expected_displacement}, \code{expected_rms}, \code{centre_shift},
#'   \code{mean_pairwise_before}, \code{mean_pairwise_after},
#'   \code{mean_pairwise_change}, \code{noise_radius}, \code{n},
#'   \code{seed}, \code{method}.
#' @references Allshouse, W. B., Fitch, M. K., Hampton, K. H., Gesink,
#'   D. C., Doherty, I. A., Leone, P. A., Serre, M. L. & Miller, W. C.
#'   (2010). Geomasking sensitive health data and privacy protection: an
#'   evaluation using an E911 database. Geocarto International 25(6),
#'   443-452. \doi{10.1080/10106049.2010.496496}
#' @export
#' @examples
#' set.seed(1)
#' Sdcdis(matrix(runif(20), 10, 2), noise_radius = 0.1)
Sdcdis <- function(coords, noise_radius, seed = 1) {
  P <- .s03mat(coords)
  n <- nrow(P)
  if (n == 0L) stop("spatial_data_distortion: coords is empty")
  if (ncol(P) != 2L)
    stop("spatial_data_distortion: coords must have exactly two columns")
  r <- as.numeric(noise_radius)
  if (r < 0) stop("spatial_data_distortion: noise_radius must be non-negative")

  rng <- .t1_lcg(seed)
  masked <- matrix(0, n, 2L)
  disp <- numeric(n)
  for (k in seq_len(n)) {
    rho <- r * sqrt(rng$unif())
    th <- 2 * pi * rng$unif()
    masked[k, 1L] <- P[k, 1L] + rho * cos(th)
    masked[k, 2L] <- P[k, 2L] + rho * sin(th)
    disp[k] <- rho
  }

  centre <- function(Q) c(sum(Q[, 1L]) / n, sum(Q[, 2L]) / n)
  c0 <- centre(P)
  c1 <- centre(masked)
  centre_shift <- sqrt((c1[1] - c0[1])^2 + (c1[2] - c0[2])^2)

  mean_pair <- function(Q) {
    if (n < 2L) return(NaN)
    s <- 0
    m <- 0L
    for (a in seq_len(n)) {
      if (a < n) for (b in seq(a + 1L, n)) {
        s <- s + sqrt((Q[a, 1L] - Q[b, 1L])^2 + (Q[a, 2L] - Q[b, 2L])^2)
        m <- m + 1L
      }
    }
    s / m
  }
  mp0 <- mean_pair(P)
  mp1 <- mean_pair(masked)

  mean_disp <- sum(disp) / n
  rms <- sqrt(sum(disp * disp) / n)

  .t1_result(estimate = mean_disp, masked = masked, displacement = disp,
             mean_displacement = mean_disp, max_displacement = max(disp),
             rms_displacement = rms, expected_displacement = 2 * r / 3,
             expected_rms = r / sqrt(2), centre_shift = centre_shift,
             mean_pairwise_before = mp0, mean_pairwise_after = mp1,
             mean_pairwise_change = mp1 - mp0, noise_radius = r, n = n,
             seed = seed,
             method = "Geomasking by uniform displacement within a disc (Allshouse et al. 2010)")
}
