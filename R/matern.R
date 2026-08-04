# SPDX-License-Identifier: AGPL-3.0-or-later
#' The Matern cluster process: Poisson centres, satellites in a disc
#'
#' Matern, B. (1960), Spatial Variation, Meddelanden fran Statens
#' Skogsforskningsinstitut 49(5), 1-144.  Section 3.6, "Models of randomly
#' located points", pp. 46-47, rendered at 150 dpi with pdftoppm and read as
#' images.
#'
#' Matern set-up, p. 46: centres follow a Poisson process of intensity
#' lambda; each centre carries a cluster of satellites whose count has mean
#' m and variance tau^2; f(x - y) is the density of a satellite position
#' about a centre at y; and gamma(u) = Integral f(u + y) f(y) dy is the
#' autoconvolution.  With Z(S) the number of satellites in S and mu the
#' volume, E[Z(S)] = lambda m mu(S), eq. (3.6.1), and
#' Cov[Z(S1), Z(S2)] = lambda m mu(S1 ^ S2)
#' + lambda (m^2 + tau^2 - m) Int Int gamma(u - y) du dy, eq. (3.6.2).
#'
#' Taking the cluster size Poisson, so tau^2 = m, the bracket collapses to
#' m^2, and comparing (3.6.2) with the general second-moment form gives
#' rho_2(u) = (lambda m)^2 + lambda m^2 gamma(u), hence
#' g(u) = 1 + gamma(u) / lambda and K(t) = pi t^2 + H(t) / lambda with
#' H(t) = Int_{|u| <= t} gamma(u) du.
#'
#' This function is the planar case n = 2 with satellites uniform on the
#' disc of radius r about their centre, the model that carries Matern name
#' today.  There f = 1 / (pi r^2) on the disc, so gamma(u) is the area
#' common to two discs of radius r whose centres are u apart, divided by
#' (pi r^2)^2.  That common area is Matern V_n(A, A; v) of eq. (3.4.4)
#' p. 38; at n = 2 it is the circular lens
#' V_2(r, r; v) = 2 r^2 acos(v / 2r) - (v / 2) sqrt(4 r^2 - v^2), zero for
#' v >= 2r and pi r^2 at v = 0.
#'
#' H(t) integrates in closed form.  Substituting v = 2 r s and S = t / 2r,
#' H(t) = (16 / pi) [ (S^2/2) acos S + (1/8) asin S - (1/8) S sqrt(1 - S^2)
#' - (1/4) S^3 sqrt(1 - S^2) ] with S = min(t / 2r, 1), and at S = 1 the
#' bracket is pi/16, so H = 1 exactly.  That is forced: gamma is the
#' autoconvolution of a probability density, so it integrates to 1 over the
#' whole plane, and two discs of radius r more than 2r apart cannot overlap.
#' Hence for every t >= 2r, K(t) = pi t^2 + 1 / lambda exactly, which is the
#' anchor this function is checked against.  It also says what the process
#' is: K exceeds the Poisson pi t^2 by 1/lambda, one whole excess neighbour
#' per parent, all of it accumulated inside 2r.
#'
#' @param lambda_p intensity lambda of the Poisson process of centres.
#' @param mu mean number m of satellites per centre.
#' @param r cluster radius; satellites are uniform on the disc of radius r.
#' @param t lags at which g and K are reported.  The default is the
#'   deterministic grid r * (0.25, 0.5, 1, 1.5, 2, 3), which straddles 2r so
#'   that the K(t) = pi t^2 + 1/lambda plateau is exercised.
#' @return list: estimate, intensity, t, g, K, gamma, H, Kpois, lambda_p,
#'   mu, r, n, method.
#' @keywords internal
#' @examples
#' Matern(5, 3, 0.1)$K
#' @export
Matern <- function(lambda_p, mu, r, t = NULL) {
  lam <- as.numeric(lambda_p)
  m <- as.numeric(mu)
  rr <- as.numeric(r)
  nms <- c("lambda_p", "mu", "r")
  vals <- c(lam, m, rr)
  for (i in seq_along(vals)) {
    if (is.na(vals[i]) || !(vals[i] > 0)) {
      stop(paste0("matern_cluster: ", nms[i], " must be positive"))
    }
  }
  if (is.null(t)) {
    tv <- rr * c(0.25, 0.5, 1, 1.5, 2, 3)
  } else {
    tv <- .s03vec(t)
    if (length(tv) == 0L) stop("matern_cluster: no lags supplied")
    for (x in tv) {
      if (is.na(x) || x < 0) stop("matern_cluster: every lag t must be non-negative")
    }
  }
  norm <- (pi * rr * rr)^2
  gam <- numeric(length(tv))
  H <- numeric(length(tv))
  for (i in seq_along(tv)) {
    gam[i] <- .s03lens(rr, tv[i]) / norm
    H[i] <- .s03lensH(tv[i], rr)
  }
  g <- 1 + gam / lam
  K <- pi * tv * tv + H / lam
  list(estimate = lam * m, intensity = lam * m, t = tv, g = g, K = K,
       gamma = gam, H = H, Kpois = pi * tv * tv, lambda_p = lam, mu = m,
       r = rr, n = length(tv),
       method = "Matern (1960) cluster process, eqs. (3.6.1)-(3.6.2) p. 46")
}

# Area common to two discs of radius r whose centres are v apart: Matern
# (1960) V_n(A, A; v) of eq. (3.4.4) p. 38, at n = 2.  Shared with Hcoreg.
.s03lens <- function(r, v) {
  if (v >= 2 * r) return(0)
  if (v <= 0) return(pi * r * r)
  q <- v / (2 * r)
  if (q > 1) q <- 1
  2 * r * r * acos(q) - 0.5 * v * sqrt(4 * r * r - v * v)
}

# Integral of gamma over the disc of radius t; 1 for t >= 2r.
.s03lensH <- function(t, r) {
  if (t <= 0) return(0)
  S <- t / (2 * r)
  if (S >= 1) return(1)
  w <- sqrt(1 - S * S)
  br <- 0.5 * S * S * acos(S) + 0.125 * asin(S) - 0.125 * S * w -
    0.25 * S * S * S * w
  16 / pi * br
}
