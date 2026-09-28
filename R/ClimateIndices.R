.ci_standardise <- function(v, groups, base) {
  out <- numeric(length(v))
  for (g in sort(unique(groups))) {
    idx <- which(groups == g)
    ref <- v[idx][base[idx]]
    out[idx] <- (v[idx] - mean(ref)) / stats::sd(ref)
  }
  out
}

#' Climate indices and CO2 curve fitting
#'
#' \code{NaoStationIndex}: station-based NAO index (Hurrell 1995).
#' \code{Co2CurveFit}: polynomial trend plus annual harmonics (Thoning et al.
#' 1989). Identical to the Python arm \code{morie.fn.climidx}.
#'
#' @param slp_south,slp_north Sea-level pressure series.
#' @param months Optional calendar month of each value.
#' @param base Optional logical mask of the base period.
#' @param t Decimal years.
#' @param co2 Concentrations.
#' @param n_poly Number of polynomial terms.
#' @param n_harm Number of harmonics.
#' @return A vector or list.
#' @references Hurrell, J. W. (1995). Decadal trends in the North Atlantic
#'   Oscillation. Science 269, 676-679.
#'
#'   Thoning, K. W., Tans, P. P. and Komhyr, W. D. (1989). Atmospheric carbon
#'   dioxide at Mauna Loa Observatory 2. Journal of Geophysical Research 94,
#'   8549-8565.
#' @examples
#' NaoStationIndex(c(1020, 1024, 1018), c(1000, 996, 1004))
#' @export
NaoStationIndex <- function(slp_south, slp_north, months = NULL, base = NULL) {
  g <- if (is.null(months)) rep(0, length(slp_south)) else months
  b <- if (is.null(base)) rep(TRUE, length(slp_south)) else as.logical(base)
  .ci_standardise(slp_south, g, b) - .ci_standardise(slp_north, g, b)
}

#' @rdname NaoStationIndex
#' @export
Co2CurveFit <- function(t, co2, n_poly = 3, n_harm = 4) {
  t0 <- sum(t) / length(t)
  u <- t - t0
  P <- outer(u, seq_len(n_poly) - 1, "^")
  H <- do.call(cbind, lapply(seq_len(n_harm), function(k) cbind(sin(2 * pi * k * t), cos(2 * pi * k * t))))
  X <- cbind(P, H)
  beta <- as.vector(solve(crossprod(X), crossprod(X, co2)))
  trend <- as.vector(P %*% beta[seq_len(n_poly)])
  seas <- as.vector(H %*% beta[-seq_len(n_poly)])
  growth <- if (n_poly > 1) as.vector(outer(u, seq_len(n_poly - 1) - 1, "^") %*% (seq_len(n_poly - 1) * beta[2:n_poly])) else
    rep(0, length(t))
  list(coefficients = beta, t0 = t0, trend = trend, seasonal = seas, residuals = co2 - trend - seas, growth_rate = growth)
}
