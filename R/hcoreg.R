# SPDX-License-Identifier: AGPL-3.0-or-later
#' Matern hard-core processes: no two points closer than R
#'
#' Matern, B. (1960), Spatial Variation, Meddelanden fran Statens
#' Skogsforskningsinstitut 49(5), 1-144.  Pages 47 and 48, rendered at
#' 150 dpi with pdftoppm and read as images, give the two models by which
#' Matern obtains sub-normal dispersion, described there as "two very simple
#' models, in which no pair of random points is allowed to have a mutual
#' distance below a certain bound".
#'
#' MODEL I, p. 47.  Realise a Poisson process of intensity lambda and then
#' "exclude every event such that the distance to its nearest neighbour is
#' less than a given positive number R"; if two points are closer than R,
#' BOTH go.  Then E[Z(S)] = alpha lambda mu(S), eq. (3.6.5) p. 48, with
#' alpha = exp(-lambda C_n R^n), eq. (3.6.6) p. 48, and the pair retention
#' function k(v) = 0 for 0 < v < R and exp(-lambda U(R, R; v)) for R <= v,
#' eq. (3.6.4) p. 47.
#'
#' MODEL II, p. 48.  The same primary process runs in time over 0 < t < 1
#' and a point survives if no other primary event fell within R of it
#' earlier.  Then alpha = (1 - exp(-lambda gamma)) / (lambda gamma),
#' eq. (3.6.8) p. 48, and k(v) = 0 for 0 < v < R, else
#' (2 U (1 - exp(-lambda gamma)) - 2 gamma (1 - exp(-lambda U)))
#' / (lambda^2 gamma U (U - gamma)), eq. (3.6.9) p. 48, with gamma the
#' volume C_n R^n of the sphere of radius R.
#'
#' U is Matern U(a, b; v) of eq. (3.4.7) p. 38, the volume of the UNION of
#' the two spheres, U(a, b; v) = C_n a^n + C_n b^n - V_n(a, b; v), V_n being
#' the volume they share, eq. (3.4.4) p. 38.  This function is the planar
#' case n = 2, where C_2 = pi and V_2 is the circular lens; that lens is
#' written once, in Matern, and reused here.
#'
#' Two consequences are used as anchors, because both are exact and neither
#' runs through the code being tested.  At v >= 2R the two discs are
#' disjoint, so U = 2 gamma, and Model I gives k = exp(-2 lambda gamma) =
#' alpha^2 while Model II gives (1 - 2 e^-x + e^-2x) / x^2 =
#' ((1 - e^-x)/x)^2 = alpha^2 with x = lambda gamma; that is, both models
#' decorrelate to independence exactly, and the Model II collapse in
#' particular only comes out if (3.6.9) has been transcribed right.  As
#' R -> 0 both alphas tend to 1 and the intensity to lambda, the Poisson
#' limit.  Model II always retains more than Model I, since
#' (1 - e^-x)/x > e^-x for every x > 0.
#'
#' The Gibbs reading.  Independently of Matern thinnings, the hard-core
#' point pattern has the unnormalised density lambda^n on configurations
#' that satisfy the constraint and 0 on those that do not; that is what the
#' density and log_density elements report for the supplied coordinates, and
#' retained is Model I own accept/reject decision per point.
#'
#' @param coords planar coordinates, as a two-column matrix of (x, y) or a
#'   flat x, y, x, y, ... vector of even length.
#' @param r the hard-core distance R; positive.
#' @param lam intensity lambda of the underlying Poisson process; positive.
#' @param model 1 or 2, which of Matern models the headline alpha,
#'   intensity and k refer to.  Both are always computed.  Default 2.
#' @return list: estimate, alpha, alpha_I, alpha_II, intensity_I,
#'   intensity_II, feasible, min_dist, density, log_density, retained,
#'   n_retained, d, k_I, k_II, gamma, r, lam, model, n, method.
#' @keywords internal
#' @examples
#' Hcoreg(matrix(c(0, 0, 3, 0, 0, 3), ncol = 2, byrow = TRUE), 1, 0.5)$alpha_I
#' @export
Hcoreg <- function(coords, r, lam, model = 2) {
  pts <- .s03pairs(coords)
  rr <- as.numeric(r)
  lm <- as.numeric(lam)
  if (is.na(rr) || !(rr > 0)) {
    stop("hardcore_process: the hard-core distance r must be positive")
  }
  if (is.na(lm) || !(lm > 0)) {
    stop("hardcore_process: the intensity lam must be positive")
  }
  if (!(identical(model, 1) || identical(model, 2) ||
        identical(model, 1L) || identical(model, 2L))) {
    stop("hardcore_process: model must be 1 or 2")
  }
  n <- nrow(pts)
  d <- numeric(0)
  close <- rep(FALSE, n)
  if (n > 1L) {
    for (i in seq_len(n - 1L)) {
      for (j in seq(i + 1L, n)) {
        dx <- pts[i, 1] - pts[j, 1]
        dy <- pts[i, 2] - pts[j, 2]
        v <- sqrt(dx * dx + dy * dy)
        d <- c(d, v)
        if (v < rr) {
          close[i] <- TRUE
          close[j] <- TRUE
        }
      }
    }
  }
  d <- sort(d)
  min_dist <- if (length(d)) d[1] else Inf
  feasible <- !any(close)
  retained <- ifelse(close, 0, 1)
  n_retained <- sum(retained)

  gam <- pi * rr * rr
  x <- lm * gam
  alpha_I <- exp(-x)
  alpha_II <- -expm1(-x) / x
  k_I <- numeric(length(d))
  k_II <- numeric(length(d))
  for (i in seq_along(d)) {
    k_I[i] <- .s03hcK1(rr, d[i], lm)
    k_II[i] <- .s03hcK2(rr, d[i], lm)
  }
  alpha <- if (as.numeric(model) == 1) alpha_I else alpha_II
  list(estimate = alpha * lm, alpha = alpha, alpha_I = alpha_I,
       alpha_II = alpha_II, intensity_I = alpha_I * lm,
       intensity_II = alpha_II * lm, feasible = feasible,
       min_dist = min_dist,
       density = if (feasible) lm^n else 0,
       log_density = if (feasible) n * log(lm) else -Inf,
       retained = retained, n_retained = n_retained, d = d, k_I = k_I,
       k_II = k_II, gamma = gam, r = rr, lam = lm,
       model = as.numeric(model), n = n,
       method = paste0("Matern (1960) hard-core models I and II, ",
                       "eqs. (3.6.4)-(3.6.9) pp. 47-48"))
}

# Coerce coords to a two-column matrix of (x, y).  Accepts a two-column
# matrix or data frame, or a flat vector of even length in x, y order.
.s03pairs <- function(coords) {
  if (is.matrix(coords) || is.data.frame(coords)) {
    m <- as.matrix(coords)
    if (ncol(m) != 2L) {
      stop("hardcore_process: every coordinate must have two components")
    }
    if (nrow(m) == 0L) stop("hardcore_process: no coordinates supplied")
    storage.mode(m) <- "double"
    dimnames(m) <- NULL
    return(m)
  }
  flat <- .s03vec(coords)
  if (length(flat) == 0L) stop("hardcore_process: no coordinates supplied")
  if (length(flat) %% 2L != 0L) {
    stop("hardcore_process: flat coords must have even length")
  }
  matrix(flat, ncol = 2L, byrow = TRUE)
}

# Matern eq. (3.4.7) p. 38 at n = 2: area of the union of two discs.
.s03hcU <- function(r, v) 2 * pi * r * r - .s03lens(r, v)

# Equation (3.6.4) p. 47.
.s03hcK1 <- function(r, v, lam) {
  if (v < r) return(0)
  exp(-lam * .s03hcU(r, v))
}

# Equation (3.6.9) p. 48.
.s03hcK2 <- function(r, v, lam) {
  if (v < r) return(0)
  gam <- pi * r * r
  U <- .s03hcU(r, v)
  # -expm1(-t) rather than 1 - exp(-t): for the small lambda*gamma that a
  # tiny hard-core radius produces, 1 - exp(-t) loses most of its significant
  # digits to cancellation, and (3.6.9) is a ratio of two such differences,
  # so the error does not divide out.
  num <- 2 * U * (-expm1(-lam * gam)) - 2 * gam * (-expm1(-lam * U))
  den <- lam * lam * gam * U * (U - gam)
  num / den
}
