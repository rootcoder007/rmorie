.lat_const <- function(W) {
  list(n = nrow(W), S0 = sum(W), S1 = 0.5 * sum((W + t(W))^2), S2 = sum((rowSums(W) + colSums(W))^2))
}

.lat_moran <- function(x, W) {
  k <- .lat_const(W)
  n <- k$n
  z <- x - sum(x) / n
  zz <- sum(z^2)
  I <- n / k$S0 * sum(z * as.vector(W %*% z)) / zz
  K <- n * sum(z^4) / zz^2
  E <- -1 / (n - 1)
  nn <- n * n
  V <- n * (k$S1 * (nn - 3 * n + 3) - n * k$S2 + 3 * k$S0^2)
  V <- V - K * (k$S1 * (nn - n) - 2 * n * k$S2 + 6 * k$S0^2)
  V <- V / ((n - 1) * (n - 2) * (n - 3) * k$S0^2) - E^2
  zs <- (I - E) / sqrt(V)
  list(statistic = I, expected = E, variance = V, z = zs, p_value = stats::pnorm(zs, lower.tail = FALSE))
}

.lat_gearyc <- function(x, W) {
  n <- length(x)
  z <- x - sum(x) / n
  zz <- sum(z^2)
  C <- (n - 1) / (2 * sum(W)) * sum(W * outer(x, x, "-")^2) / zz
  c(C, n * sum(z^4) / zz^2)
}

.lat_geary <- function(x, W, randomisation = TRUE) {
  k <- .lat_const(W)
  n <- k$n
  ck <- .lat_gearyc(x, W)
  C <- ck[1]
  K <- ck[2]
  n1 <- n - 1
  nn <- n * n
  S02 <- k$S0^2
  if (randomisation) {
    V <- n1 * k$S1 * (nn - 3 * n + 3 - K * n1)
    V <- V - 0.25 * (n1 * k$S2 * (nn + 3 * n - 6 - K * (nn - n + 2)))
    V <- V + S02 * (nn - 3 - K * n1^2)
    V <- V / (n * (n - 2) * (n - 3) * S02)
  } else {
    V <- ((2 * k$S1 + k$S2) * n1 - 4 * S02) / (2 * (n + 1) * S02)
  }
  zs <- (1 - C) / sqrt(V)
  list(statistic = C, expected = 1, variance = V, z = zs, p_value = stats::pnorm(zs, lower.tail = FALSE), K = K)
}

.lat_getis <- function(x, W) {
  if (min(x) < 0) stop("Getis-Ord G needs non-negative x")
  k <- .lat_const(W)
  n <- k$n
  sx <- sum(x)
  sx2 <- sum(x^2)
  sx3 <- sum(x^3)
  sx4 <- sum(x^4)
  G <- sum(x * as.vector(W %*% x)) / (sx^2 - sx2)
  EG <- k$S0 / (n * (n - 1))
  nn <- n * n
  S1 <- k$S1
  S2 <- k$S2
  S02 <- k$S0^2
  B0 <- (nn - 3 * n + 3) * S1 - n * S2 + 3 * S02
  B1 <- -((nn - n) * S1 - 2 * n * S2 + 6 * S02)
  B2 <- -(2 * n * S1 - (n + 3) * S2 + 6 * S02)
  B3 <- 4 * (n - 1) * S1 - 2 * (n + 1) * S2 + 8 * S02
  B4 <- S1 - S2 + S02
  VG <- (B0 * sx2^2 + B1 * sx4 + B2 * sx^2 * sx2 + B3 * sx * sx3 + B4 * sx^4) /
    ((sx^2 - sx2)^2 * n * (n - 1) * (n - 2) * (n - 3)) - EG^2
  zs <- (G - EG) / sqrt(VG)
  list(statistic = G, expected = EG, variance = VG, z = zs, p_value = stats::pnorm(zs, lower.tail = FALSE))
}

.lat_local <- function(x, W) {
  n <- length(x)
  z <- x - sum(x) / n
  m2 <- sum(z^2) / n
  lz <- as.vector(W %*% z)
  Ii <- z / m2 * lz
  Wi <- unname(rowSums(W))
  Wi2 <- unname(rowSums(W^2))
  E <- -(z^2 * Wi) / ((n - 1) * m2)
  V <- (z / m2)^2 * (n / (n - 2)) * (Wi2 - Wi^2 / (n - 1)) * (m2 - z^2 / (n - 1))
  Z <- ifelse(V > 0, (Ii - E) / sqrt(pmax(V, 0)), NaN)
  list(Ii = Ii, expected = E, variance = V, z = Z, p_value = 2 * stats::pnorm(abs(Z), lower.tail = FALSE))
}

.lat_quadrants <- function(x, W) {
  z <- x - sum(x) / length(x)
  lz <- as.vector(W %*% z)
  q <- ifelse(z > 0, ifelse(lz > 0, 1L, 4L), ifelse(lz > 0, 2L, 3L))
  list(q = q, z = z, lz = lz)
}

.lat_lee <- function(x, y, W) {
  n <- length(x)
  zx <- x - sum(x) / n
  zy <- y - sum(y) / n
  lx <- as.vector(W %*% zx)
  ly <- as.vector(W %*% zy)
  s <- sqrt(sum(zx^2)) * sqrt(sum(zy^2))
  list(L = n / sum(rowSums(W)^2) * sum(lx * ly) / s, local = n * lx * ly / s)
}

#' Lattice spatial autocorrelation statistics
#'
#' Global and local spatial autocorrelation on a lattice in \code{spdep}
#' conventions; R arm of the Python modules \code{morie.fn.lac*}.
#' \code{Lacgear}: Geary's C with randomisation or normality moments
#' (\code{spdep::geary.test}). \code{Lacgetg}: global Getis-Ord G
#' (\code{spdep::globalG.test}). \code{Lacgmc}: Geary's C permutation test,
#' permutations as the order of Philox uniforms, pseudo p-value
#' (1 + number of permuted C at most C) / (nsim + 1). \code{Laclisa}: local Moran's
#' \eqn{I_i} with the conditional randomisation moments
#' (\code{spdep::localmoran} defaults). \code{Laclihh}, \code{Laclihl}:
#' significant high-high clusters and high-low outliers. \code{Laclimc}:
#' conditional permutation pseudo p-values of \eqn{I_i}, folded
#' (1 + smaller count of permuted values at least or at most the observed
#' one) / (nsim + 1).
#' \code{Lacscat}: Moran scatterplot quadrants (1 HH, 2 LH, 3 LL, 4 HL) and
#' slope. \code{Lacscor}: correlation of y with Wy and Lee's
#' \eqn{L_{yy}}. \code{Lacbivl}: Lee's bivariate L (\code{spdep::lee}).
#' \code{Lactest}: Moran, Geary and Getis-Ord global tests.
#' \code{Lacvgm}: Matheron semivariogram with \code{gstat} default bins.
#'
#' @param y Variable on the n units.
#' @param W Spatial weights matrix (zero diagonal).
#' @param randomisation Randomisation (TRUE) or normality variance.
#' @param nsim Number of permutations.
#' @param seed Philox seed.
#' @param p_thr Significance threshold for the local tests.
#' @param x Second variable (\code{Lacbivl}).
#' @param coords n x 2 matrix of unit coordinates.
#' @param n_lags Number of distance bins.
#' @param cutoff Largest distance considered (default one third of the
#'   bounding-box diagonal).
#' @return A list whose \code{statistic} is the headline value, with the
#'   components of the Python result.
#' @references Cliff, A. D. and Ord, J. K. (1981). Spatial Processes: Models
#'   and Applications. Pion, London.
#'
#'   Geary, R. C. (1954). The contiguity ratio and statistical mapping. The
#'   Incorporated Statistician 5, 115-145.
#'
#'   Getis, A. and Ord, J. K. (1992). The analysis of spatial association by
#'   use of distance statistics. Geographical Analysis 24, 189-206.
#'
#'   Anselin, L. (1995). Local indicators of spatial association -- LISA.
#'   Geographical Analysis 27, 93-115.
#'
#'   Sokal, R. R., Oden, N. L. and Thomson, B. A. (1998). Local spatial
#'   autocorrelation in a biological model. Geographical Analysis 30,
#'   331-354.
#'
#'   Lee, S.-I. (2001). Developing a bivariate spatial association measure.
#'   Journal of Geographical Systems 3, 369-385.
#'
#'   Cressie, N. (1993). Statistics for Spatial Data. Wiley, New York.
#' @examples
#' W <- 1 * (abs(outer(1:6, 1:6, "-")) == 1)
#' W <- W / rowSums(W)
#' y <- c(1, 2.4, 1.3, 3.1, 1.9, 2.2)
#' Lacgear(y, W)$statistic
#' Laclisa(y, W)$local_values
#' Lactest(y, W)$p_value
#' @export
Lacgear <- function(y, W, randomisation = TRUE) {
  .lat_geary(as.numeric(y), as.matrix(W), randomisation)
}

#' @rdname Lacgear
#' @export
Lacgetg <- function(y, W) {
  .morie_arg(y, "n")
  .lat_getis(as.numeric(y), as.matrix(W))
}

#' @rdname Lacgear
#' @export
Lacgmc <- function(y, W, nsim = 99, seed = 0) {
  y <- as.numeric(y)
  W <- as.matrix(W)
  n <- length(y)
  C <- .lat_gearyc(y, W)[1]
  u <- .morie_random_uniform(nsim * n, seed = seed)
  sims <- vapply(seq_len(nsim), function(s) .lat_gearyc(y[order(u[(s - 1) * n + seq_len(n)])], W)[1], numeric(1))
  list(statistic = C, p_value = (1 + sum(sims <= C)) / (nsim + 1), simulated = sims, mean = mean(sims))
}

#' @rdname Lacgear
#' @export
Laclisa <- function(y, W) {
  W <- as.matrix(W)
  r <- .lat_local(as.numeric(y), W)
  list(statistic = sum(r$Ii) / sum(W), local_values = r$Ii, expected = r$expected,
       variance = r$variance, z = r$z, p_value = r$p_value)
}

.lat_cluster <- function(y, W, p_thr, code) {
  y <- as.numeric(y)
  W <- as.matrix(W)
  r <- .lat_local(y, W)
  q <- .lat_quadrants(y, W)$q
  idx <- which(q == code & r$p_value < p_thr) - 1L
  list(statistic = length(idx), local_values = r$Ii, indices = idx, p_value = r$p_value, Ii = r$Ii, quadrant = q)
}

#' @rdname Lacgear
#' @export
Laclihh <- function(y, W, p_thr = 0.05) {
  .lat_cluster(y, W, p_thr, 1L)
}

#' @rdname Lacgear
#' @export
Laclihl <- function(y, W, p_thr = 0.05) {
  .lat_cluster(y, W, p_thr, 4L)
}

#' @rdname Lacgear
#' @export
Laclimc <- function(y, W, nsim = 99, seed = 0) {
  y <- as.numeric(y)
  W <- as.matrix(W)
  n <- length(y)
  z <- y - sum(y) / n
  m2 <- sum(z^2) / n
  Ii <- z / m2 * as.vector(W %*% z)
  u <- .morie_random_uniform(nsim * n * (n - 1), seed = seed)
  p <- numeric(n)
  ms <- numeric(n)
  sds <- numeric(n)
  for (i in seq_len(n)) {
    oth <- seq_len(n)[-i]
    sims <- vapply(seq_len(nsim), function(s) {
      blk <- u[((s - 1) * n + i - 1) * (n - 1) + seq_len(n - 1)]
      z[i] / m2 * sum(W[i, oth] * z[oth[order(blk)]])
    }, numeric(1))
    p[i] <- (1 + min(sum(sims >= Ii[i]), sum(sims <= Ii[i]))) / (nsim + 1)
    ms[i] <- mean(sims)
    sds[i] <- stats::sd(sims)
  }
  list(statistic = sum(p < 0.05), local_values = Ii, p_value = p, mean_sim = ms, sd_sim = sds)
}

#' @rdname Lacgear
#' @export
Lacscat <- function(y, W) {
  q <- .lat_quadrants(as.numeric(y), as.matrix(W))
  list(statistic = sum(q$z * q$lz) / sum(q$z^2), local_values = q$q,
       counts = tabulate(q$q, 4L), z = q$z, lag = q$lz)
}

#' @rdname Lacgear
#' @export
Lacscor <- function(y, W) {
  y <- as.numeric(y)
  W <- as.matrix(W)
  wy <- as.vector(W %*% y)
  a <- y - sum(y) / length(y)
  b <- wy - sum(wy) / length(wy)
  list(statistic = sum(a * b) / sqrt(sum(a^2) * sum(b^2)), lee_L = .lat_lee(y, y, W)$L)
}

#' @rdname Lacgear
#' @export
Lacbivl <- function(x, y, W) {
  r <- .lat_lee(as.numeric(x), as.numeric(y), as.matrix(W))
  list(statistic = r$L, local_values = r$local)
}

#' @rdname Lacgear
#' @export
Lactest <- function(y, W) {
  y <- as.numeric(y)
  W <- as.matrix(W)
  m <- .lat_moran(y, W)
  list(statistic = m$statistic, p_value = m$p_value, expected = m$expected, variance = m$variance,
       moran = m, geary = .lat_geary(y, W), getis_ord = if (min(y) >= 0) .lat_getis(y, W) else NULL)
}

#' @rdname Lacgear
#' @export
Lacvgm <- function(y, coords, n_lags = 15, cutoff = NULL) {
  y <- as.numeric(y)
  coords <- as.matrix(coords)
  if (is.null(cutoff)) cutoff <- sqrt(sum(apply(coords[, 1:2], 2, function(v) diff(range(v)))^2)) / 3
  w <- cutoff / n_lags
  d <- as.matrix(stats::dist(coords[, 1:2]))
  up <- upper.tri(d) & d <= cutoff & d > 0
  k <- pmin(ceiling(d[up] / w) - 1, n_lags - 1) + 1
  g <- outer(y, y, "-")[up]^2
  np <- tabulate(k, n_lags)
  keep <- np > 0
  dist <- vapply(seq_len(n_lags), function(j) sum(d[up][k == j]), numeric(1))
  gam <- vapply(seq_len(n_lags), function(j) sum(g[k == j]), numeric(1))
  gamma <- gam[keep] / (2 * np[keep])
  list(statistic = if (length(gamma)) gamma[1] else NaN, np = np[keep], dist = dist[keep] / np[keep],
       gamma = gamma, cutoff = cutoff, width = w)
}
