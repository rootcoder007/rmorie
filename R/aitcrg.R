# SPDX-License-Identifier: AGPL-3.0-or-later
#' Ordinary least squares in ILR coordinates (compositional regression)
#'
#' The composition-valued response is carried into real coordinates by the
#' isometric log-ratio map, ordinary least squares is run there, and the fit is
#' carried back to the simplex: Y_ilr = ilr(Y), Y_ilr = X B + E,
#' Yhat = ilr^-1(X B).  This is legitimate precisely because ilr is an isometry
#' of the Aitchison geometry onto Euclidean space, so the least-squares
#' criterion in coordinates is the Aitchison-distance criterion on the simplex;
#' that equivalence is the point of Pawlowsky-Glahn, Egozcue and
#' Tolosana-Delgado (2015), Modeling and Analysis of Compositional Data, Wiley,
#' and it is checked as an anchor rather than assumed.
#'
#' The default basis is the sequential binary partition of Egozcue,
#' Pawlowsky-Glahn, Mateu-Figueras and Barcelo-Vidal (2003), "Isometric
#' logratio transformations for compositional data analysis", Mathematical
#' Geology 35(3), 279-300, doi:10.1023/a:1023818214614 (verified against
#' Crossref), equation (11) as rendered from the page image.
#'
#' X is used exactly as supplied; if an intercept is wanted, put a column of
#' ones in it.  Nothing is added silently.
#'
#' @param X N-by-p design matrix, used verbatim.
#' @param Y_comp N-by-D matrix of strictly positive compositions.
#' @param V optional D-by-(D-1) contrast matrix; defaults to the Egozcue et al.
#'   (2003) sequential binary partition.
#' @return list: beta, fitted, resid, fitted_comp, Y_ilr, sse, estimate, N, p,
#'   D, method.
#' @keywords internal
#' @examples
#' Aitcrg(cbind(1, c(1, 2, 3)), rbind(c(.2, .3, .5), c(.3, .3, .4), c(.4, .3, .3)))$beta
#' @export
Aitcrg <- function(X, Y_comp, V = NULL) {
  Xm <- as.matrix(X)
  storage.mode(Xm) <- "double"
  Ym <- as.matrix(Y_comp)
  storage.mode(Ym) <- "double"
  N <- nrow(Xm)
  if (N == 0L || nrow(Ym) == 0L) stop("compositional_regression: no observations")
  if (nrow(Ym) != N) stop("compositional_regression: X and Y_comp have different row counts")
  p <- ncol(Xm)
  D <- ncol(Ym)
  if (D < 2L) stop("compositional_regression: a composition needs at least 2 parts")
  if (any(!(Ym > 0))) stop("compositional_regression: every part of Y_comp must be positive")
  if (N < p) stop("compositional_regression: fewer observations than columns of X")
  Vm <- if (is.null(V)) {
    .aitcrg_basis(D)
  } else {
    matrix(as.numeric(as.matrix(V)),
      nrow = nrow(as.matrix(V))
    )
  }
  q <- ncol(Vm)
  Yi <- matrix(0, nrow = N, ncol = q)
  for (n in seq_len(N)) Yi[n, ] <- .aitcrg_ilr(Ym[n, ], Vm)
  beta <- matrix(0, nrow = p, ncol = q)
  for (cc in seq_len(q)) {
    b <- .s03lstsq(Xm, Yi[, cc], ridge = 0)
    for (j in seq_len(p)) beta[j, cc] <- b[j]
  }
  fitted <- matrix(0, nrow = N, ncol = q)
  resid <- matrix(0, nrow = N, ncol = q)
  sse <- 0
  for (n in seq_len(N)) {
    for (cc in seq_len(q)) {
      s <- 0
      for (j in seq_len(p)) s <- s + Xm[n, j] * beta[j, cc]
      fitted[n, cc] <- s
      resid[n, cc] <- Yi[n, cc] - s
      sse <- sse + resid[n, cc] * resid[n, cc]
    }
  }
  fitted_comp <- matrix(0, nrow = N, ncol = D)
  for (n in seq_len(N)) fitted_comp[n, ] <- .aitcrg_inv(fitted[n, ], Vm)
  list(
    beta = beta, fitted = fitted, resid = resid, fitted_comp = fitted_comp,
    Y_ilr = Yi, sse = sse, estimate = beta[1, 1], N = N, p = p, D = D,
    method = "OLS of ilr(Y) on X in the Egozcue et al. (2003) SBP basis"
  )
}

#' @noRd
.aitcrg_basis <- function(D) {
  V <- matrix(0, nrow = D, ncol = D - 1L)
  for (i in seq_len(D - 1L)) {
    cc <- sqrt(i / (i + 1))
    for (j in seq_len(i)) V[j, i] <- cc / i
    V[i + 1L, i] <- -cc
  }
  V
}

#' @noRd
.aitcrg_ilr <- function(x, V) {
  lg <- log(x)
  z <- lg - sum(lg) / length(lg)
  out <- numeric(ncol(V))
  for (i in seq_len(ncol(V))) {
    s <- 0
    for (j in seq_len(nrow(V))) s <- s + V[j, i] * z[j]
    out[i] <- s
  }
  out
}

#' @noRd
.aitcrg_inv <- function(y, V) {
  lx <- numeric(nrow(V))
  for (j in seq_len(nrow(V))) {
    s <- 0
    for (i in seq_along(y)) s <- s + V[j, i] * y[i]
    lx[j] <- s
  }
  e <- exp(lx - max(lx))
  e / sum(e)
}
