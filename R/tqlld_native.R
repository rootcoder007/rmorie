# Lloyd-Max optimal scalar quantiser.
# Sources: Lloyd, S. P. (1982) Least squares quantization in PCM, IEEE
# Transactions on Information Theory 28(2), 129-137 (written 1957);
# Max, J. (1960) Quantizing for minimum distortion, IRE Transactions
# on Information Theory 6(1), 7-12. The two conditions -- decision
# boundary midway between codewords, codeword equal to the conditional
# mean of its cell -- give the Lloyd-Max algorithm, with monotonic
# distortion decrease.
#
# Native implementation mirroring Python morie.fn.tqlld exactly: the
# same three sources (gaussian, empirical, uniform), the same closed
# form for the uniform case, the same empirical cell assignment by a
# single index advance, the same gaussian-cell mass/moment by
# midpoint quadrature, the same validation messages.

.SOURCES <- c("gaussian", "empirical", "uniform")

#' .tqlld_phi
#'
#' A step of the tqlld_native implementation. Called by \code{.gaussian_cells}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.tqlld_phi <- function(x) exp(-0.5 * x * x) / sqrt(2 * pi)

#' .gaussian_cells
#'
#' A step of the tqlld_native implementation. Called by \code{morie_tqlld}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param bounds Passed to \code{c}.
#' @param lo Numeric; combined arithmetically in the body.
#' @param hi Numeric; combined arithmetically in the body.
#' @param n_grid Numeric; combined arithmetically in the body.
#' @return A list with \code{mass}, \code{mom}.
#' @export
.gaussian_cells <- function(bounds, lo, hi, n_grid) {
  edges <- c(lo, bounds, hi)
  mass <- mom <- numeric(length(edges) - 1L)
  for (k in seq_len(length(edges) - 1L)) {
    a <- edges[k]
    b <- edges[k + 1L]
    if (b <= a) next
    m <- max(2L, as.integer(n_grid * (b - a) / (hi - lo)) + 2L)
    h <- (b - a) / m
    for (i in seq_len(m)) {
      x <- a + (i - 0.5) * h
      w <- .tqlld_phi(x) * h
      mass[k] <- mass[k] + w
      mom[k] <- mom[k] + x * w
    }
  }
  list(mass = mass, mom = mom)
}

#' Optimal levels-point scalar codebook
#'
#' @param levels Number of codewords.
#' @param source One of \code{"gaussian"}, \code{"empirical"},
#'   \code{"uniform"}.
#' @param data Samples for the empirical source.
#' @param lo Lower bound for the uniform source.
#' @param hi Upper bound for the uniform source.
#' @param max_iter Maximum iterations.
#' @param tol Convergence tolerance.
#' @param n_grid Quadrature grid size for the Gaussian source.
#' @return A list with \code{estimate}, \code{codebook},
#'   \code{boundaries}, \code{distortion},
#'   \code{distortion_history}, \code{iterations}, \code{converged},
#'   \code{source}, \code{levels}, \code{lo}, \code{hi} (uniform
#'   only), \code{method}.
#' @references Lloyd, S. P. (1982); Max, J. (1960).
#' @export
morie_tqlld <- function(levels = 4, source = "gaussian", data = NULL,
                         lo = NULL, hi = NULL, max_iter = 200,
                         tol = 1e-12, n_grid = 20000) {
  N <- as.integer(levels)
  if (N < 1L) stop("lloyd_max_codebook: levels must be >= 1")
  src <- tolower(as.character(source))
  if (!src %in% .SOURCES)
    stop("lloyd_max_codebook: source must be one of gaussian, empirical, uniform")
  if (src == "uniform") {
    a <- if (is.null(lo)) -1 else as.numeric(lo)
    b <- if (is.null(hi)) 1 else as.numeric(hi)
    if (b <= a) stop("lloyd_max_codebook: need hi > lo")
    w <- (b - a) / N
    cb <- a + ((seq_len(N)) - 0.5) * w
    bnd <- a + (seq_len(N - 1L)) * w
    dist <- w^2 / 12
    return(list(estimate = cb, codebook = cb, boundaries = bnd,
                distortion = dist, distortion_history = c(dist),
                iterations = 0L, converged = TRUE, source = src,
                levels = N, lo = a, hi = b,
                method = "Uniform-source Lloyd-Max, closed form (Lloyd 1982; Max 1960)"))
  }
  if (src == "empirical") {
    if (is.null(data))
      stop("lloyd_max_codebook: empirical source needs data")
    xs <- sort(as.numeric(data))
    if (length(xs) == 0L)
      stop("lloyd_max_codebook: empirical source needs data")
    if (length(xs) < N)
      stop("lloyd_max_codebook: fewer samples than levels")
    cb <- vapply(seq_len(N), function(k)
      xs[min(length(xs), as.integer((k - 0.5) * length(xs) / N) + 1L)],
      numeric(1))
  } else {
    LO <- -8
    HI <- 8
    cb <- -3 + 6 * (seq_len(N) - 0.5) / N
  }
  hist <- numeric(0)
  it <- 0L
  converged <- FALSE
  prev <- Inf
  for (it in seq_len(as.integer(max_iter))) {
    cb <- sort(cb)
    bnd <- 0.5 * (cb[seq_len(N - 1L)] + cb[seq_len(N - 1L) + 1L])
    if (src == "empirical") {
      cells <- vector("list", N)
      j <- 1L
      for (x in xs) {
        while (j < N && x > bnd[j]) j <- j + 1L
        cells[[j]] <- c(cells[[j]], x)
      }
      new <- vapply(seq_len(N), function(k)
        if (length(cells[[k]]) > 0L) mean(cells[[k]]) else cb[k],
        numeric(1))
      dist <- sum(sapply(seq_len(N), function(k)
        sum((cells[[k]] - new[k])^2))) / length(xs)
    } else {
      mc <- .gaussian_cells(bnd, LO, HI, n_grid)
      mass <- mc$mass
      mom <- mc$mom
      new <- vapply(seq_len(N), function(k)
        if (mass[k] > 1e-300) mom[k] / mass[k] else cb[k],
        numeric(1))
      dist <- 1 - sum(mom * new)
    }
    hist <- c(hist, dist)
    shift <- max(abs(new - cb))
    cb <- new
    if (shift <= tol || abs(prev - dist) <= tol) { converged <- TRUE
    break }
    prev <- dist
  }
  cb <- sort(cb)
  bnd <- 0.5 * (cb[seq_len(N - 1L)] + cb[seq_len(N - 1L) + 1L])
  list(estimate = cb, codebook = cb, boundaries = bnd,
       distortion = if (length(hist) > 0L) hist[length(hist)] else 0,
       distortion_history = hist, iterations = it,
       converged = converged, source = src, levels = N,
       method = "Lloyd-Max alternating nearest-neighbour and centroid conditions (Lloyd 1982; Max 1960)")
}

#' Map each sample to its nearest codeword
#'
#' @param x Samples.
#' @param codebook Codebook.
#' @return A list with \code{estimate}, \code{indices}, \code{values},
#'   \code{mse}, \code{levels}, \code{method}.
#' @export
morie_quantize_with_codebook <- function(x, codebook) {
  cb <- as.numeric(codebook)
  if (length(cb) == 0L) stop("quantize_with_codebook: codebook is empty")
  xv <- as.numeric(x)
  idx <- integer(length(xv))
  val <- numeric(length(xv))
  for (i in seq_along(xv)) {
    v <- xv[i]
    best <- 1L
    bd <- abs(v - cb[1L])
    for (k in 2:length(cb)) {
      d <- abs(v - cb[k])
      if (d < bd) { bd <- d
      best <- k }
    }
    idx[i] <- best - 1L
    val[i] <- cb[best]
  }
  mse <- mean((xv - val)^2)
  list(estimate = val, indices = idx, values = val, mse = mse,
       levels = length(cb),
       method = "Nearest-codeword quantisation")
}

#' Public alias resolved by fn/_lazy_map.json
#' @export
#' @noRd
morie_lloyd_max_codebook <- morie_tqlld
