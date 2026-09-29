#' Spatial weights construction
#'
#' \code{GridContiguity}: queen or rook contiguity of a regular grid, cells
#' numbered row by row. \code{DistanceBandWeights}: \code{d1 < d <= d2}
#' neighbours. \code{KnnWeights}: k nearest neighbours (ties to the lower
#' index). \code{InverseDistanceWeights}: \code{d^-power} weights.
#' \code{KernelWeights}: fixed or adaptive geographically weighted kernels as
#' \code{GWmodel::gw.weight}. \code{SymmetrizeWeights}: union, intersection or
#' average symmetrisation. \code{WeightsComponents}: connected components as
#' \code{spdep::n.comp.nb}. Identical to the Python arm \code{morie.fn.swbuild}.
#'
#' @param nrow,ncol Grid dimensions.
#' @param type \code{"queen"} or \code{"rook"}.
#' @param torus Wrap the grid edges.
#' @param coords Two-column coordinates.
#' @param d1,d2 Distance band.
#' @param k Number of neighbours.
#' @param power Distance decay exponent.
#' @param row_standardize Row-standardise the weights.
#' @param bw Bandwidth (distance, or number of points when adaptive).
#' @param kernel \code{"gaussian"}, \code{"exponential"}, \code{"bisquare"},
#'   \code{"tricube"} or \code{"boxcar"}.
#' @param adaptive Adaptive bandwidth.
#' @param diagonal Keep the self weights.
#' @param W Weights matrix.
#' @param method \code{"union"}, \code{"intersection"} or \code{"average"}.
#' @return Matrix or list.
#' @references Bivand, R. S., Pebesma, E. and Gomez-Rubio, V. (2013). Applied
#'   Spatial Data Analysis with R, 2nd ed. Springer.
#'
#'   Gollini, I., Lu, B., Charlton, M., Brunsdon, C. and Harris, P. (2015).
#'   GWmodel: an R package for exploring spatial heterogeneity using
#'   geographically weighted models. Journal of Statistical Software 63(17).
#' @examples
#' GridContiguity(2, 2, type = "rook")
#' KernelWeights(rbind(c(0, 0), c(1, 0), c(3, 0)), 2)[1, ]
#' @export
GridContiguity <- function(nrow, ncol, type = "queen", torus = FALSE) {
  if (!type %in% c("queen", "rook")) stop("type must be queen or rook")
  n <- nrow * ncol
  W <- matrix(0, n, n)
  for (r in 0:(nrow - 1)) {
    for (cc in 0:(ncol - 1)) {
      for (dr in -1:1) {
        for (dc in -1:1) {
          if ((dr == 0 && dc == 0) || (type == "rook" && dr != 0 && dc != 0)) next
          rr <- r + dr
          c2 <- cc + dc
          if (torus) {
            rr <- rr %% nrow
            c2 <- c2 %% ncol
          } else if (rr < 0 || rr >= nrow || c2 < 0 || c2 >= ncol) {
            next
          }
          j <- rr * ncol + c2 + 1
          i <- r * ncol + cc + 1
          if (j != i) W[i, j] <- 1
        }
      }
    }
  }
  W
}

#' @rdname GridContiguity
#' @export
DistanceBandWeights <- function(coords, d2, d1 = 0) {
  D <- unname(as.matrix(stats::dist(as.matrix(coords))))
  W <- (D > d1 & D <= d2) + 0
  diag(W) <- 0
  W
}

#' @rdname GridContiguity
#' @export
KnnWeights <- function(coords, k) {
  D <- unname(as.matrix(stats::dist(as.matrix(coords))))
  n <- nrow(D)
  if (k < 1 || k >= n) stop("k must be in 1..n-1")
  W <- matrix(0, n, n)
  for (i in seq_len(n)) {
    o <- setdiff(order(D[i, ], seq_len(n)), i)
    W[i, o[seq_len(k)]] <- 1
  }
  W
}

#' @rdname GridContiguity
#' @export
InverseDistanceWeights <- function(coords, power = 1, d2 = NULL, row_standardize = FALSE) {
  D <- unname(as.matrix(stats::dist(as.matrix(coords))))
  W <- ifelse(D == 0, 0, D^(-power))
  if (!is.null(d2)) W[D > d2] <- 0
  diag(W) <- 0
  if (row_standardize) {
    s <- rowSums(W)
    # ifelse() takes its shape from the length-n test, so it returned the
    # first n entries of W / s as a bare vector; divide row-wise instead
    W <- W / ifelse(s > 0, s, 1)
  }
  W
}

#' @rdname GridContiguity
#' @export
KernelWeights <- function(coords, bw, kernel = "bisquare", adaptive = FALSE, diagonal = TRUE) {
  D <- unname(as.matrix(stats::dist(as.matrix(coords))))
  n <- nrow(D)
  if (adaptive && (bw < 1 || bw > n)) stop("adaptive bw must be between 1 and n")
  out <- t(vapply(seq_len(n), function(i) {
    b <- if (adaptive) sort(D[i, ])[floor(bw)] else bw
    u <- D[i, ] / b
    switch(kernel,
      gaussian = exp(-0.5 * u^2), exponential = exp(-u),
      bisquare = ifelse(D[i, ] < b, (1 - u^2)^2, 0), tricube = ifelse(D[i, ] < b, (1 - u^3)^3, 0),
      boxcar = ifelse(D[i, ] <= b, 1, 0), stop("unknown kernel")
    )
  }, numeric(n)))
  if (!diagonal) diag(out) <- 0
  out
}

#' @rdname GridContiguity
#' @export
SymmetrizeWeights <- function(W, method = "union") {
  A <- as.matrix(W)
  switch(method,
    union = ((A != 0) | (t(A) != 0)) + 0,
    intersection = ((A != 0) & (t(A) != 0)) + 0,
    average = (A + t(A)) / 2,
    stop("method must be union, intersection or average")
  )
}

#' @rdname GridContiguity
#' @export
WeightsComponents <- function(W) {
  A <- as.matrix(W)
  A <- (A != 0) | (t(A) != 0)
  n <- nrow(A)
  comp <- integer(n)
  cc <- 0L
  for (s in seq_len(n)) {
    if (comp[s]) next
    cc <- cc + 1L
    comp[s] <- cc
    stack <- s
    while (length(stack)) {
      i <- stack[length(stack)]
      stack <- stack[-length(stack)]
      nb <- which(A[i, ] & comp == 0L)
      comp[nb] <- cc
      stack <- c(stack, nb)
    }
  }
  list(n_components = cc, component = comp, connected = cc == 1L, sizes = tabulate(comp, cc))
}
