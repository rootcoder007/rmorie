#' Generalized secant hyperbolic distribution
#'
#' Vaughan's standardised family (mean 0, variance 1) with density
#' f(z) = c1 exp(c2 z) / (exp(2 c2 z) + 2 a exp(c2 z) + 1), x = loc + scale z.
#' For -pi < t < 0: a = cos t, c2 = sqrt((pi^2 - t^2)/3), c1 = sin(t)/t c2;
#' t = 0 is the logistic; for t > 0 cosh and sinh replace cos and sin and
#' pi^2 + t^2 replaces pi^2 - t^2. The distribution function and quantile are
#' closed form.
#'
#' @param x Points for pdf, logpdf and cdf.
#' @param t Shape, t > -pi; t = -pi/2 is the hyperbolic secant distribution.
#' @param loc,scale Location and scale.
#' @param p Probabilities for the quantile.
#' @param n Number of draws (Philox stream, matches the Python arm).
#' @param seed Seed.
#' @return list with pdf, logpdf, cdf, quantile, random as requested.
#' @references Vaughan, D. C. (2002). The generalized secant hyperbolic
#'   distribution and its properties. Communications in Statistics - Theory and
#'   Methods 31, 219-238.
#' @examples
#' GHSecant(c(-1, 0, 1), t = -pi / 2)$pdf
#' @export
GHSecant <- function(x = NULL, t = 0, loc = 0, scale = 1, p = NULL, n = 0, seed = 0) {
  if (!(t > -pi && scale > 0)) stop("need t > -pi and scale > 0", call. = FALSE)
  if (t < 0) {
    a <- cos(t)
    c2 <- sqrt((pi^2 - t^2) / 3)
    c1 <- sin(t) / t * c2
  } else if (t == 0) {
    a <- 1
    c2 <- pi / sqrt(3)
    c1 <- c2
  } else {
    a <- cosh(t)
    c2 <- sqrt((pi^2 + t^2) / 3)
    c1 <- sinh(t) / t * c2
  }
  pdf <- function(v) {
    e <- exp(-abs(c2 * (v - loc) / scale))
    c1 * e / (1 + 2 * a * e + e^2) / scale
  }
  cdf <- function(v) {
    y <- c2 * (v - loc) / scale
    if (t == 0) return(plogis(y))
    if (t < 0) return(1 + pi / (2 * t) + atan((exp(pmin(y, 700)) + cos(t)) / sin(t)) / t)
    e <- exp(-abs(y))
    ifelse(y > 0, 1 + log((1 + exp(-t) * e) / (1 + exp(t) * e)) / (2 * t),
           1 + log((e + exp(-t)) / (e + exp(t))) / (2 * t))
  }
  qf <- function(u) {
    y <- if (t == 0) {
      qlogis(u)
    } else if (t < 0) {
      log(-sin(t) / tan(t * (u - 1)) - cos(t))
    } else {
      r <- exp(2 * t * (u - 1))
      log((r * exp(t) - exp(-t)) / (1 - r))
    }
    loc + scale * y / c2
  }
  res <- list()
  if (!is.null(x)) {
    dens <- pdf(x)
    res$pdf <- dens
    res$logpdf <- ifelse(dens > 0, log(dens), -Inf)
    res$cdf <- cdf(x)
  }
  if (!is.null(p)) res$quantile <- qf(p)
  if (n > 0) res$random <- qf(.morie_random_uniform(n, seed = seed, stream = 0))
  res
}

#' Bingham distribution on the circle and the sphere
#'
#' Density exp(x' A x) / c(A) for unit vectors x (surface measure), p = 2 or 3.
#' With eigenvalues l1 >= l2 (>= l3) of the symmetric A, p = 2 has
#' c(A) = 2 pi exp((l1 + l2)/2) I0((l1 - l2)/2); p = 3 integrates the azimuth
#' analytically, c(A) = 2 pi times the integral over z in (-1, 1) of exp(l3 z^2 + (l1 + l2)(1 - z^2)/2)
#' I0((l1 - l2)(1 - z^2)/2) dz, by composite Gauss-Legendre.
#'
#' @param x A unit vector or a matrix of unit vectors in rows.
#' @param A Symmetric 2 x 2 or 3 x 3 parameter matrix.
#' @return list(pdf, logpdf, log_normalizer).
#' @references Bingham, C. (1974). An antipodally symmetric distribution on the
#'   sphere. Annals of Statistics 2, 1201-1225.
#' @examples
#' BinghamDens(c(1, 0, 0), diag(c(2, 0, -1)))$pdf
#' @export
BinghamDens <- function(x, A) {
  A <- as.matrix(A)
  d <- nrow(A)
  if (!(d %in% c(2, 3)) || ncol(A) != d || max(abs(A - t(A))) > 1e-12) {
    stop("A must be a symmetric 2 x 2 or 3 x 3 matrix", call. = FALSE)
  }
  lam <- sort(eigen(A, symmetric = TRUE, only.values = TRUE)$values, decreasing = TRUE)
  li0 <- function(z) vapply(abs(z), function(q) if (q > 0) .mv_log_bessel_i(0, q) else 0, 0)
  if (d == 2) {
    logc <- log(2 * pi) + (lam[1] + lam[2]) / 2 + li0((lam[1] - lam[2]) / 2)
  } else {
    shift <- max(lam[1], lam[3])
    f <- function(z) exp(lam[3] * z^2 + (lam[1] + lam[2]) * (1 - z^2) / 2 + li0((lam[1] - lam[2]) * (1 - z^2) / 2) - shift)
    logc <- log(2 * pi) + shift + log(.dist_gl(f, -1, 1, n = max(64L, as.integer(8 * (abs(lam[1]) + abs(lam[3]))))))
  }
  P <- .mv_points(x)
  if (ncol(P) != d || any(abs(rowSums(P^2) - 1) > 1e-9)) stop("x must be unit vectors of the dimension of A", call. = FALSE)
  lp <- rowSums((P %*% A) * P) - logc
  list(pdf = exp(lp), logpdf = lp, log_normalizer = logc)
}
