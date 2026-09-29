#' Eigenvector and Getis spatial filtering utilities
#'
#' `sfgetis` is the Getis (1995) filter (`GetisFilter`); `spatial_filter`
#' forward-selects Moran eigenvectors (`EigenvectorFiltering`); `sfloc`
#' returns the fitted filter `E_S gamma` and its value at unit `i`; `sfmemb`
#' keeps the Moran eigenvector maps whose simple-regression t test on `y` is
#' significant at `alpha / m` (Bonferroni over the `m` candidates); `sfmi`
#' is Moran's I of filtered residuals with randomisation moments; `sforth`
#' checks the orthonormality of selected eigenvectors and their
#' orthogonality to the constant; `sfredu` reports the residual Moran's I
#' before and after filtering.  Eigenvector indices are 0-based, as in the
#' Python arm.
#'
#' @param y Response.
#' @param data Response of `spatial_filter`.
#' @param W Spatial weights matrix.
#' @param X Optional regressors (intercept added).
#' @param i 0-based unit index.
#' @param method,criterion Selection criterion: "moran", "aic", "press" or "r2".
#' @param tol Stopping tolerance of the Moran criterion.
#' @param alpha Family-wise level.
#' @param candidates "positive", "negative" or "all" eigenvectors.
#' @param resid_f,resid Residual vectors.
#' @param alternative Alternative hypothesis for the Moran test.
#' @param evecs Matrix whose columns are eigenvectors.
#' @return Lists (see the Python arm for the components).
#' @references Getis, A. (1995). Spatial filtering in a regression
#'   framework. In L. Anselin and R. Florax (eds), New Directions in Spatial
#'   Econometrics. Springer, 172-185. Griffith, D. A. (2003). Spatial
#'   Autocorrelation and Spatial Filtering. Springer. Tiefelsdorf, M. and
#'   Griffith, D. A. (2007). Semiparametric filtering of spatial
#'   autocorrelation: the eigenvector approach. Environment and Planning A
#'   39, 1193-1221. Dray, S., Legendre, P. and Peres-Neto, P. R. (2006).
#'   Spatial modelling: a comprehensive framework for principal coordinate
#'   analysis of neighbour matrices (PCNM). Ecological Modelling 196,
#'   483-493. Bauman, D., Drouet, T., Dray, S. and Vleminckx, J. (2018).
#'   Disentangling good from bad practices in the selection of spatial or
#'   phylogenetic eigenvectors. Ecography 41, 1638-1649.
#' @examples
#' W <- 1 * (abs(outer(1:8, 1:8, "-")) == 1)
#' y <- c(1, 2, 2.5, 4, 3.5, 3, 1.5, 1)
#' spatial_filter(y, W, method = "aic")$selected
#' sfmemb(y, W, alpha = 0.2)$selected
#' @export
sfgetis <- function(y, W) GetisFilter(y, W)

#' @rdname sfgetis
#' @export
spatial_filter <- function(data, W, X = NULL, method = "moran", tol = 0.1) {
  r <- EigenvectorFiltering(data, W, X, criterion = method, tol = tol)
  r$selected <- r$selected - 1L
  r
}

#' @rdname sfgetis
#' @export
sfloc <- function(y, X, W, i = 0, criterion = "moran", tol = 0.1) {
  r <- EigenvectorFiltering(y, W, X, criterion = criterion, tol = tol)
  E <- MoranEigenvectors(W)$vectors
  sel <- r$selected
  gam <- utils::tail(r$coefficients, length(sel))
  filt <- if (length(sel)) as.vector(E[, sel, drop = FALSE] %*% gam) else rep(0, nrow(E))
  list(filter = filt, value = filt[i + 1], selected = sel - 1L, coefficients = gam)
}

#' @rdname sfgetis
#' @export
sfmemb <- function(y, W, alpha = 0.05, candidates = "positive") {
  y <- as.numeric(y)
  n <- length(y)
  me <- MoranEigenvectors(W)
  pool <- switch(candidates, positive = me$positive, negative = me$negative, all = seq_along(me$eigenvalues))
  m <- length(pool)
  yc <- y - mean(y)
  r <- as.vector(crossprod(me$vectors[, pool, drop = FALSE], yc)) / sqrt(sum(yc^2))
  tt <- r * sqrt((n - 2) / (1 - r^2))
  p <- 2 * stats::pt(abs(tt), n - 2, lower.tail = FALSE)
  sel <- pool[p < alpha / m]
  list(selected = sel - 1L, p_values = p, t = tt, candidates = pool - 1L, threshold = alpha / m,
       coefficients = as.vector(crossprod(me$vectors[, sel, drop = FALSE], y)))
}

#' @rdname sfgetis
#' @export
sfmi <- function(resid_f, W, alternative = "greater") miml(resid_f, W, alternative = alternative)

#' @rdname sfgetis
#' @export
sforth <- function(evecs, tol = 1e-8) {
  E <- unname(as.matrix(evecs)) * 1
  dev <- max(abs(crossprod(E) - diag(ncol(E))))
  const <- max(abs(colSums(E))) / sqrt(nrow(E))
  list(statistic = dev, max_constant_projection = const, orthonormal = dev < tol && const < tol)
}

#' @rdname sfgetis
#' @export
sfredu <- function(resid, W, tol = 0.1) {
  i0 <- EigenvectorFiltering(resid, W, max_vectors = 0)$residual_moran
  r <- EigenvectorFiltering(resid, W, criterion = "moran", tol = tol)
  i1 <- r$residual_moran
  list(moran_before = i0, moran_after = i1, reduction = i0 - i1,
       relative_reduction = if (i0 != 0) (i0 - i1) / i0 else NaN, selected = r$selected - 1L)
}
