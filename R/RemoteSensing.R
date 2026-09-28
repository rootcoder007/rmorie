.rs_tc <- list(
  landsat5tm = rbind(c(0.2043, -0.1603, 0.0315), c(0.4158, -0.2819, 0.2021), c(0.5524, -0.4934, 0.3102),
                     c(0.5741, 0.7940, 0.1594), c(0.3124, 0.0002, 0.6806), c(0.2303, -0.1446, -0.6109)),
  landsat7etm = rbind(c(0.3561, -0.3344, 0.2626), c(0.3972, -0.3544, 0.2141), c(0.3904, -0.4556, 0.0926),
                      c(0.6966, 0.6966, 0.0656), c(0.2286, -0.0242, -0.7629), c(0.1596, -0.2630, -0.5388)),
  landsat8oli = rbind(c(0.3029, -0.2941, 0.1511), c(0.2786, -0.2430, 0.1973), c(0.4733, -0.5424, 0.3283),
                      c(0.5599, 0.7276, 0.3407), c(0.5080, 0.0713, -0.7117), c(0.1872, -0.1608, -0.4559)),
  modis = rbind(c(0.4395, -0.4064, 0.1147), c(0.5945, 0.5129, 0.2489), c(0.2460, -0.2744, 0.2408),
                c(0.3918, -0.2893, 0.3132), c(0.3506, 0.4882, -0.3122), c(0.2136, -0.0036, -0.6416),
                c(0.2678, -0.4169, -0.5087)),
  quickbird = rbind(c(0.319, 0.542, 0.490), c(-0.121, -0.331, -0.517), c(0.652, 0.375, -0.639),
                    c(0.677, -0.675, 0.292)),
  spot5 = rbind(c(0.492, -0.196, 0.397), c(0.610, -0.389, 0.260), c(0.416, 0.896, 0.118), c(0.462, -0.084, -0.872)),
  rapideye = rbind(c(0.2435, -0.2216, -0.7564), c(0.3448, -0.2319, -0.3916), c(0.4881, -0.4622, 0.5049),
                   c(0.4930, -0.2154, 0.1400), c(0.5835, 0.7981, 0.0064))
)
.rs_tc$landsat4tm <- .rs_tc$landsat5tm

.rs_grids <- function(bands) {
  G <- lapply(bands, function(b) matrix(as.numeric(as.matrix(b)), nrow(as.matrix(b))))
  d <- dim(G[[1]])
  if (!all(vapply(G, function(g) identical(dim(g), d), TRUE))) stop("bands must be equal-shape 2-D grids", call. = FALSE)
  G
}

# pixel x band matrix, row-major pixel order (as the Python arm)
.rs_pixels <- function(G) vapply(G, function(g) as.vector(t(g)), numeric(length(G[[1]])))

.rs_to_grids <- function(P, nr, nc) lapply(seq_len(ncol(P)), function(b) matrix(P[, b], nr, nc, byrow = TRUE))

.rs_eigsym <- function(A) {
  e <- eigen((A + t(A)) / 2, symmetric = TRUE)
  V <- e$vectors
  for (i in seq_len(ncol(V))) if (V[which.max(abs(V[, i])), i] < 0) V[, i] <- -V[, i]
  list(values = e$values, vectors = V)
}

#' Multispectral image processing (RStoolbox conventions)
#'
#' Bands are equal-shape matrices (a list). \code{TasseledCap}: brightness,
#' greenness, wetness with the sensor coefficients of
#' \code{RStoolbox::tasseledCap}. \code{BandPca}: principal components
#' (covariance or, \code{spca}, correlation; scores of the centred, scaled
#' pixels; loadings signed so their largest entry is positive).
#' \code{MnfTransform}: minimum noise fraction with the shift-difference noise
#' estimate (Green et al. 1988). \code{PanSharpen}: Brovey, IHS (intensity
#' replaced by mean/sd-matched pan) or PCA (PC1 replaced by pan stretched to
#' PC1's range) on bands already on the pan grid.
#' \code{TopographicCorrection}: cos, avgcos, Minnaert, C and statistical
#' corrections from the illumination \eqn{\cos i}. \code{EstimateHaze} and
#' \code{RadiometricCorrection}: dark-object haze and DN to radiance, TOA or
#' DOS / COST surface reflectance (Chavez 1988, 1996). \code{CloudMask}
#' (NDTCI) and \code{CloudShadowMask} (the cloud mask shifted by the sun
#' geometry). \code{SpectralAngleClassify}, \code{GaussianMlClassify}
#' (quadratic discriminant, priors from training proportions),
#' \code{KmeansClassify} (Lloyd from given centres), \code{AtgpEndmembers}
#' (Ren and Chang 2003), \code{FparFromNdvi} (Los et al. 2000) and
#' \code{Tvdi} (Sandholt et al. 2002). Classes and indices are 1-based.
#' Identical to the Python arm \code{morie.fn.rsimage}.
#'
#' @param bands,ms List of equal-shape band matrices.
#' @param sensor Tasseled-cap sensor.
#' @param spca Use the correlation matrix.
#' @param n_comp Number of components.
#' @param pan Panchromatic matrix on the same grid.
#' @param method Method name.
#' @param rgb Indices of the red, green and blue bands.
#' @param slope,aspect Slope and aspect matrices (radians).
#' @param sun_azimuth,sun_zenith Sun angles (radians).
#' @param dn DN matrix (haze) or list of DN matrices.
#' @param dark_prop Dark-object proportion.
#' @param max_slope Use the steepest rise of the smoothed histogram.
#' @param gain,offset Radiance rescaling per band.
#' @param esun Exo-atmospheric solar irradiance per band.
#' @param sun_elevation Sun elevation (degrees).
#' @param distance Earth-sun distance (AU).
#' @param haze_dn Haze DN (one value, or one per band for \code{"sdos"}).
#' @param haze_band Index of the haze band (\code{"dos"}, \code{"costz"}).
#' @param wavelengths Band centre wavelengths.
#' @param atmosphere Relative scattering class, or \code{NULL} to choose.
#' @param radiometric_bits Radiometric resolution.
#' @param clamp Clamp reflectance to the unit interval (radiance to >= 0).
#' @param view_zenith Sensor view zenith (degrees) for \code{"costz"}'s view
#'   transmittance \eqn{\cos\theta_v}; 1 as \code{RStoolbox}, 0 for strict nadir.
#' @param blue,tir Blue and thermal matrices.
#' @param threshold NDTCI threshold.
#' @param cloud Logical cloud mask.
#' @param shift Shift \code{c(dx, dy)} in cells (dy north).
#' @param dark Optional darkness matrix (e.g. band sum).
#' @param quantile Darkness quantile.
#' @param endmembers Endmember spectra (rows).
#' @param train Training pixels (rows, bands as columns).
#' @param labels Training labels.
#' @param priors Class priors (sorted label order).
#' @param centers Initial centres (rows).
#' @param max_iter Maximum iterations.
#' @param n Number of endmembers.
#' @param ndvi,lst NDVI and land surface temperature vectors.
#' @param ndvi_min,ndvi_max,fpar_min,fpar_max FPAR scaling constants.
#' @param n_bins Number of NDVI bins.
#' @param ndvi_range NDVI range for the bins.
#' @return List.
#' @references Crist, E. P. (1985). A TM tasseled cap equivalent transformation
#'   for reflectance factor data. Remote Sensing of Environment 17, 301-306.
#'
#'   Baig, M. H. A., Zhang, L., Shuai, T. and Tong, Q. (2014). Derivation of a
#'   tasselled cap transformation based on Landsat 8 at-satellite reflectance.
#'   Remote Sensing Letters 5, 423-431.
#'
#'   Green, A. A., Berman, M., Switzer, P. and Craig, M. D. (1988). A
#'   transformation for ordering multispectral data in terms of image quality.
#'   IEEE Transactions on Geoscience and Remote Sensing 26, 65-74.
#'
#'   Teillet, P. M., Guindon, B. and Goodenough, D. G. (1982). On the
#'   slope-aspect correction of multispectral scanner data. Canadian Journal of
#'   Remote Sensing 8, 84-106.
#'
#'   Chavez, P. S. (1988). An improved dark-object subtraction technique for
#'   atmospheric scattering correction of multispectral data. Remote Sensing of
#'   Environment 24, 459-479.
#'
#'   Ren, H. and Chang, C.-I. (2003). Automatic spectral target recognition in
#'   hyperspectral imagery. IEEE Transactions on Aerospace and Electronic
#'   Systems 39, 1232-1249.
#'
#'   Los, S. O. et al. (2000). A global 9-yr biophysical land surface dataset
#'   from NOAA AVHRR data. Journal of Hydrometeorology 1, 183-199.
#'
#'   Sandholt, I., Rasmussen, K. and Andersen, J. (2002). A simple
#'   interpretation of the surface temperature/vegetation index space for
#'   assessment of surface moisture status. Remote Sensing of Environment 79,
#'   213-224.
#' @examples
#' px <- function(v) matrix(v, 1, 1)
#' TasseledCap(lapply(c(0.1, 0.1, 0.1, 0.3, 0.2, 0.1), px))$brightness
#' Tvdi(c(0.1, 0.1, 0.9, 0.9), c(300, 320, 295, 305), n_bins = 2)$tvdi
#' @export
TasseledCap <- function(bands, sensor = "landsat8oli") {
  s <- tolower(sensor)
  if (!s %in% names(.rs_tc)) stop("unknown sensor", call. = FALSE)
  C <- .rs_tc[[s]]
  G <- .rs_grids(bands)
  if (length(G) != nrow(C)) stop(s, " needs ", nrow(C), " bands", call. = FALSE)
  out <- lapply(1:3, function(k) Reduce(`+`, lapply(seq_along(G), function(b) G[[b]] * C[b, k])))
  names(out) <- c("brightness", "greenness", "wetness")
  out
}

#' @rdname TasseledCap
#' @export
BandPca <- function(bands, spca = FALSE, n_comp = NULL) {
  G <- .rs_grids(bands)
  P <- .rs_pixels(G)
  n <- nrow(P)
  mu <- colMeans(P)
  C <- stats::cov(P)
  if (spca) {
    sd <- sqrt(diag(C))
    C <- C / outer(sd, sd)
    scale <- sqrt(sd^2 * (n - 1) / n)
  } else {
    scale <- rep(1, ncol(P))
  }
  e <- .rs_eigsym(C)
  m <- if (is.null(n_comp)) ncol(P) else min(n_comp, ncol(P))
  S <- sweep(sweep(P, 2, mu), 2, scale, "/") %*% e$vectors[, seq_len(m), drop = FALSE]
  list(scores = .rs_to_grids(S, nrow(G[[1]]), ncol(G[[1]])), loadings = t(e$vectors[, seq_len(m), drop = FALSE]),
       sdev = sqrt(pmax(e$values, 0)), center = unname(mu), scale = unname(scale))
}

#' @rdname TasseledCap
#' @export
MnfTransform <- function(bands, n_comp = NULL) {
  G <- .rs_grids(bands)
  nr <- nrow(G[[1]])
  nc <- ncol(G[[1]])
  D <- vapply(G, function(g) as.vector(t(g[, -nc, drop = FALSE] - g[, -1, drop = FALSE])), numeric(nr * (nc - 1)))
  N <- stats::cov(D) / 2
  P <- .rs_pixels(G)
  mu <- colMeans(P)
  S <- stats::cov(P)
  en <- .rs_eigsym(N)
  W <- en$vectors %*% diag(1 / sqrt(en$values), length(en$values))
  es <- .rs_eigsym(t(W) %*% S %*% W)
  L <- t(W %*% es$vectors)
  for (q in seq_len(nrow(L))) if (L[q, which.max(abs(L[q, ]))] < 0) L[q, ] <- -L[q, ]
  m <- if (is.null(n_comp)) ncol(P) else min(n_comp, ncol(P))
  Sc <- sweep(P, 2, mu) %*% t(L[seq_len(m), , drop = FALSE])
  list(scores = .rs_to_grids(Sc, nr, nc), loadings = L[seq_len(m), , drop = FALSE], snr = es$values[seq_len(m)],
       noise_cov = unname(N))
}

#' @rdname TasseledCap
#' @export
PanSharpen <- function(ms, pan, method = "brovey", rgb = 1:3) {
  G <- .rs_grids(ms)
  pan <- as.matrix(pan)
  if (method == "brovey") {
    tot <- Reduce(`+`, G[rgb])
    return(list(bands = lapply(G[rgb], function(g) g * pan / tot)))
  }
  if (method == "ihs") {
    r <- G[[rgb[1]]]
    g <- G[[rgb[2]]]
    b <- G[[rgb[3]]]
    I <- (r + g + b) / 3
    v1 <- (r + g - 2 * b) / sqrt(6)
    v2 <- (r - g) / sqrt(2)
    Im <- (pan - mean(pan)) / stats::sd(as.vector(pan)) * stats::sd(as.vector(I)) + mean(I)
    return(list(bands = list(Im + v1 / sqrt(6) + v2 / sqrt(2), Im + v1 / sqrt(6) - v2 / sqrt(2),
                             Im - 2 * v1 / sqrt(6))))
  }
  if (method == "pca") {
    pc <- BandPca(G)
    S <- vapply(pc$scores, function(s) as.vector(t(s)), numeric(length(pan)))
    fp <- as.vector(t(pan))
    S[, 1] <- (fp - min(fp)) / (max(fp) - min(fp)) * (max(S[, 1]) - min(S[, 1])) + min(S[, 1])
    X <- S %*% pc$loadings + matrix(pc$center, nrow(S), length(G), byrow = TRUE)
    return(list(bands = .rs_to_grids(X, nrow(pan), ncol(pan))))
  }
  stop("method must be brovey, ihs or pca", call. = FALSE)
}

.rs_ols <- function(x, y) {
  b <- sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)
  c(mean(y) - b * mean(x), b)
}

#' @rdname TasseledCap
#' @export
TopographicCorrection <- function(bands, slope, aspect, sun_azimuth, sun_zenith, method = "C") {
  method <- match.arg(method, c("C", "cos", "avgcos", "minnaert", "stat"))
  G <- .rs_grids(bands)
  slope <- as.matrix(slope)
  il <- cos(sun_zenith) * cos(slope) + sin(sun_zenith) * sin(slope) * cos(sun_azimuth - as.matrix(aspect))
  cz <- cos(sun_zenith)
  coef <- list()
  out <- lapply(G, function(g) {
    switch(method,
      cos = g * cz / il,
      avgcos = {
        m <- mean(il)
        g + g * (m - il) / m
      },
      minnaert = {
        sel <- which(slope > 2 * pi / 180 & il >= 0)
        y <- log(ifelse(g[sel] > 0, g[sel], 1e-32))
        k <- min(1, max(0, .rs_ols(log(il[sel] / cz), y)[2]))
        coef[[length(coef) + 1]] <<- k
        g * cz / il^k
      },
      C = {
        b <- .rs_ols(as.vector(il), as.vector(g))
        coef[[length(coef) + 1]] <<- b
        ck <- b[1] / b[2]
        g * (cz + ck) / (il + ck)
      },
      stat = {
        b <- .rs_ols(as.vector(il), as.vector(g))
        coef[[length(coef) + 1]] <<- b
        g - b[2] * il
      })
  })
  list(bands = out, illumination = il, coefficients = coef)
}

#' @rdname TasseledCap
#' @export
EstimateHaze <- function(dn, dark_prop = 0.01, max_slope = TRUE) {
  v <- as.numeric(as.matrix(dn))
  v <- v[!is.na(v) & v > 0]
  tab <- table(v)
  keys <- as.numeric(names(tab))
  o <- order(keys)
  keys <- keys[o]
  freq <- as.numeric(tab)[o] / length(v)
  idx <- utils::tail(which(cumsum(freq) < dark_prop), 1)
  if (!length(idx)) idx <- 1L
  if (!(max_slope && idx > 1)) return(keys[idx])
  n <- 2 * floor((idx / 10) / 2) + 1
  sm <- stats::filter(freq[seq_len(idx)], rep(1 / n, n), sides = 2)
  keys[min(which.max(diff(sm, 2)) + 1, idx)]
}

.rs_atmos <- c(veryClear = 4, clear = 2, moderate = 1, hazy = 0.7, veryHazy = 0.5)

#' @rdname TasseledCap
#' @export
RadiometricCorrection <- function(dn, gain, offset, method = "apref", esun = NULL, sun_elevation = 90, distance = 1,
                                  haze_dn = NULL, haze_band = 1L, wavelengths = NULL, atmosphere = NULL,
                                  radiometric_bits = 8, clamp = TRUE, view_zenith = 1) {
  G <- .rs_grids(dn)
  k <- length(G)
  if (method == "rad") {
    return(list(bands = lapply(seq_len(k), function(b) {
      x <- gain[b] * G[[b]] + offset[b]
      if (clamp) pmax(x, 0) else x
    })))
  }
  method <- match.arg(method, c("apref", "sdos", "dos", "costz"))
  ct <- cos((90 - sun_elevation) * pi / 180)
  tz <- if (method == "costz") ct else 1
  tv <- if (method == "costz") cos(view_zenith * pi / 180) else 1
  lhaze <- numeric(k)
  chosen <- NULL
  if (method == "sdos") {
    ldo <- 0.01 * esun * ct / (pi * distance^2)
    lhaze <- pmax(0, (haze_dn * gain + offset) - ldo)
  } else if (method %in% c("dos", "costz")) {
    h <- haze_band
    ldo <- 0.01 * esun[h] * ct * tz * tv / (pi * distance^2)
    lh <- haze_dn[1] * gain[h] + offset[h] - ldo
    chosen <- atmosphere
    if (is.null(chosen)) {
      top <- 2^radiometric_bits - 1
      lo <- c(0, 56, 76, 96, 116) / 255 * top
      hi <- c(55, 75, 95, 115, 255) / 255 * top
      hit <- names(.rs_atmos)[lh > lo & lh <= hi]
      if (!length(hit)) stop("haze radiance outside the atmosphere table; pass `atmosphere`", call. = FALSE)
      chosen <- hit[1]
    }
    lhaze <- pmax(0, lh * (wavelengths[h] / wavelengths)^.rs_atmos[[chosen]] * gain / gain[h] + offset)
  }
  out <- lapply(seq_len(k), function(b) {
    C <- pi * distance^2 / (tv * esun[b] * ct * tz)
    x <- C * gain[b] * G[[b]] + C * (offset[b] - lhaze[b])
    if (clamp) pmin(pmax(x, 0), 1) else x
  })
  list(bands = out, haze_radiance = lhaze, atmosphere = chosen)
}

#' @rdname TasseledCap
#' @export
CloudMask <- function(blue, tir, threshold = 0.2) {
  blue <- as.matrix(blue)
  tir <- as.matrix(tir)
  tn <- (tir - min(tir)) / (max(tir) - min(tir)) * (max(blue) - min(blue)) + min(blue)
  nd <- (blue - tn) / (blue + tn)
  list(mask = nd >= threshold, ndtci = nd)
}

#' @rdname TasseledCap
#' @export
CloudShadowMask <- function(cloud, shift, dark = NULL, quantile = NULL) {
  cm <- as.matrix(cloud) != 0
  nr <- nrow(cm)
  nc <- ncol(cm)
  dx <- round(shift[1])
  dy <- round(shift[2])
  M <- matrix(FALSE, nr, nc)
  for (i in seq_len(nr)) for (j in seq_len(nc)) {
    si <- i + dy
    sj <- j - dx
    if (si >= 1 && si <= nr && sj >= 1 && sj <= nc) M[i, j] <- cm[si, sj]
  }
  if (!is.null(dark)) {
    D <- as.matrix(dark)
    fv <- sort(as.vector(D))
    q <- if (is.null(quantile)) Inf else fv[min(length(fv), floor(quantile * (length(fv) - 1)) + 1)]
    M <- M & D < q & !cm
  }
  list(mask = M)
}

#' @rdname TasseledCap
#' @export
SpectralAngleClassify <- function(bands, endmembers) {
  G <- .rs_grids(bands)
  P <- .rs_pixels(G)
  E <- as.matrix(endmembers)
  A <- vapply(seq_len(nrow(E)), function(c) {
    e <- E[c, ]
    acos(pmax(-1, pmin(1, as.vector(P %*% e) / (sqrt(sum(e^2)) * sqrt(rowSums(P^2))))))
  }, numeric(nrow(P)))
  A <- matrix(A, nrow(P))
  cls <- apply(A, 1, which.min)
  nr <- nrow(G[[1]])
  nc <- ncol(G[[1]])
  list(classes = matrix(cls, nr, nc, byrow = TRUE), angles = .rs_to_grids(A, nr, nc))
}

#' @rdname TasseledCap
#' @export
GaussianMlClassify <- function(bands, train, labels, priors = NULL) {
  G <- .rs_grids(bands)
  P <- .rs_pixels(G)
  X <- as.matrix(train)
  cls <- sort(unique(labels))
  st <- lapply(seq_along(cls), function(i) {
    R <- X[labels == cls[i], , drop = FALSE]
    S <- stats::cov(R)
    pr <- if (is.null(priors)) nrow(R) / nrow(X) else priors[i]
    list(m = colMeans(R), Si = solve(S), cst = log(pr) - 0.5 * as.numeric(determinant(S)$modulus))
  })
  sc <- vapply(st, function(s) {
    d <- sweep(P, 2, s$m)
    s$cst - 0.5 * rowSums((d %*% s$Si) * d)
  }, numeric(nrow(P)))
  sc <- matrix(sc, nrow(P))
  post <- exp(sc - apply(sc, 1, max))
  post <- post / rowSums(post)
  nr <- nrow(G[[1]])
  nc <- ncol(G[[1]])
  list(classes = matrix(cls[apply(sc, 1, which.max)], nr, nc, byrow = TRUE), posterior = post, levels = cls)
}

#' @rdname TasseledCap
#' @export
KmeansClassify <- function(bands, centers, max_iter = 100L) {
  G <- .rs_grids(bands)
  P <- .rs_pixels(G)
  C <- as.matrix(centers)
  assign <- rep(0L, nrow(P))
  it <- 0L
  for (it in seq_len(max_iter)) {
    D <- vapply(seq_len(nrow(C)), function(c) rowSums(sweep(P, 2, C[c, ])^2), numeric(nrow(P)))
    new <- apply(matrix(D, nrow(P)), 1, which.min)
    if (identical(new, assign)) {
      it <- it - 1L
      break
    }
    assign <- new
    for (c in seq_len(nrow(C))) if (any(assign == c)) C[c, ] <- colMeans(P[assign == c, , drop = FALSE])
  }
  wss <- vapply(seq_len(nrow(C)), function(c) sum(sweep(P[assign == c, , drop = FALSE], 2, C[c, ])^2), 0)
  list(classes = matrix(assign, nrow(G[[1]]), ncol(G[[1]]), byrow = TRUE), centers = unname(C), withinss = wss,
       iterations = it)
}

#' @rdname TasseledCap
#' @export
AtgpEndmembers <- function(bands, n) {
  G <- .rs_grids(bands)
  P <- .rs_pixels(G)
  idx <- integer(0)
  for (s in seq_len(n)) {
    R <- if (length(idx)) {
      U <- t(P[idx, , drop = FALSE])
      P - t(U %*% solve(crossprod(U), t(P %*% U)))
    } else P
    idx <- c(idx, which.max(rowSums(R^2)))
  }
  nc <- ncol(G[[1]])
  list(index = idx, position = cbind(row = (idx - 1) %/% nc + 1, col = (idx - 1) %% nc + 1),
       spectra = P[idx, , drop = FALSE])
}

#' @rdname TasseledCap
#' @export
FparFromNdvi <- function(ndvi, ndvi_min = 0.05, ndvi_max = 0.95, fpar_min = 0.001, fpar_max = 0.95,
                         method = "average") {
  method <- match.arg(method, c("average", "ndvi", "sr"))
  sr <- function(v) (1 + v) / (1 - v)
  lin <- function(v, lo, hi) (v - lo) * (fpar_max - fpar_min) / (hi - lo) + fpar_min
  fn <- lin(ndvi, ndvi_min, ndvi_max)
  fs <- lin(sr(ndvi), sr(ndvi_min), sr(ndvi_max))
  f <- switch(method, ndvi = fn, sr = fs, average = (fn + fs) / 2)
  list(fpar = pmin(fpar_max, pmax(fpar_min, f)))
}

#' @rdname TasseledCap
#' @export
Tvdi <- function(ndvi, lst, n_bins = 10L, ndvi_range = NULL) {
  rg <- if (is.null(ndvi_range)) range(ndvi) else ndvi_range
  w <- (rg[2] - rg[1]) / n_bins
  xs <- ys <- numeric(0)
  for (b in seq_len(n_bins) - 1) {
    ix <- which((ndvi >= rg[1] + b * w & ndvi < rg[1] + (b + 1) * w) | (b == n_bins - 1 & ndvi == rg[2]))
    if (length(ix)) {
      xs <- c(xs, mean(ndvi[ix]))
      ys <- c(ys, max(lst[ix]))
    }
  }
  ab <- .rs_ols(xs, ys)
  tmin <- min(lst)
  list(tvdi = (lst - tmin) / (ab[1] + ab[2] * ndvi - tmin), dry_edge = ab, t_min = tmin)
}
