#' Radial basis function surfaces
#'
#' \code{RadialBasis}: kernel values (linear, cubic, quintic, thin-plate,
#' polyharmonic, multiquadric family, Gaussian, Wendland).
#' \code{RbfInterpolate}: interpolation or smoothing in any dimension with
#' polynomial augmentation and optional anisotropic transform.
#' \code{RbfLoocv}: Rippa leave-one-out errors per shape parameter.
#' \code{RbfMultiscale}: multiscale Wendland residual fitting.
#' \code{RbfGrid}: grid evaluation. Identical to the Python arm
#' \code{morie.fn.rbfsurf}.
#'
#' @param r Distances.
#' @param kernel Kernel name.
#' @param epsilon Shape parameter.
#' @param order Polyharmonic order.
#' @param support Wendland support radius.
#' @param X,Xnew Data and prediction coordinates (matrices).
#' @param y Data values.
#' @param degree Polynomial degree (-1 for none).
#' @param smoothing Smoothing parameter.
#' @param transform Coordinate transform matrix.
#' @param epsilons Candidate shape parameters.
#' @param supports Decreasing Wendland supports.
#' @param xs,ys Grid axes.
#' @param ... Options passed to \code{RbfInterpolate}.
#' @return Numeric or list.
#' @references Fasshauer, G. E. (2007). Meshfree Approximation Methods with
#'   MATLAB. World Scientific.
#'
#'   Rippa, S. (1999). An algorithm for selecting a good value for the
#'   parameter c in radial basis function interpolation. Advances in
#'   Computational Mathematics 11, 193-210.
#'
#'   Floater, M. S. and Iske, A. (1996). Multistep scattered data
#'   interpolation using compactly supported radial basis functions. Journal
#'   of Computational and Applied Mathematics 73, 65-78.
#' @examples
#' RadialBasis(c(0, 0.5, 1), "wendland")
#' RbfInterpolate(matrix(0:2), c(0, 1, 0), matrix(0.5), kernel = "cubic")$prediction
#' @export
RadialBasis <- function(r, kernel = "thin_plate", epsilon = 1, order = 3, support = 1) {
  e <- epsilon * r
  switch(kernel,
    linear = -r,
    cubic = r^3,
    quintic = -r^5,
    thin_plate = ifelse(r > 0, r^2 * log(pmax(r, 1e-300)), 0),
    polyharmonic = if (order %% 2) (-1)^((order + 1) %/% 2) * r^order else
      ifelse(r > 0, (-1)^(order %/% 2 + 1) * r^order * log(pmax(r, 1e-300)), 0),
    multiquadric = -sqrt(1 + e^2),
    inverse_multiquadric = 1 / sqrt(1 + e^2),
    inverse_quadratic = 1 / (1 + e^2),
    gaussian = exp(-e^2),
    wendland = ifelse(r / support < 1, (1 - r / support)^4 * (4 * r / support + 1), 0),
    stop("unknown kernel")
  )
}

.rbf_monomials <- function(dim, degree) {
  if (degree < 0) return(list())
  out <- list(integer(0))
  if (degree >= 1) {
    for (k in seq_len(degree)) {
      cm <- as.matrix(expand.grid(rep(list(seq_len(dim)), k)))
      cm <- cm[apply(cm, 1, function(v) all(diff(v) >= 0)), , drop = FALSE]
      cm <- cm[do.call(order, as.data.frame(cm)), , drop = FALSE]
      out <- c(out, lapply(seq_len(nrow(cm)), function(i) unname(cm[i, ])))
    }
  }
  out
}

.rbf_poly <- function(p, mons) vapply(mons, function(m) if (length(m)) prod(p[m]) else 1, 0)

#' @rdname RadialBasis
#' @export
RbfInterpolate <- function(X, y, Xnew, kernel = "thin_plate", epsilon = 1, degree = 1, smoothing = 0, order = 3,
                           support = 1, transform = NULL) {
  P <- as.matrix(X)
  Q <- as.matrix(Xnew)
  if (!is.null(transform)) {
    P <- P %*% t(transform)
    if (nrow(Q)) Q <- Q %*% t(transform)
  }
  n <- nrow(P)
  mons <- .rbf_monomials(ncol(P), degree)
  m <- length(mons)
  D <- as.matrix(stats::dist(P))
  A <- RadialBasis(D, kernel, epsilon, order, support)
  A <- matrix(A, n, n) + diag(smoothing, n)
  Pm <- if (m) t(vapply(seq_len(n), function(i) .rbf_poly(P[i, ], mons), numeric(m))) else matrix(0, n, 0)
  if (m == 1) Pm <- matrix(Pm, n, 1)
  S <- rbind(cbind(A, Pm), cbind(t(Pm), matrix(0, m, m)))
  rhs <- c(y, numeric(m))
  sol <- solve(S, rhs)
  w <- sol[seq_len(n)]
  cc <- sol[n + seq_len(m)]
  pred <- vapply(seq_len(nrow(Q)), function(k) {
    dq <- sqrt(colSums((t(P) - Q[k, ])^2))
    sum(w * RadialBasis(dq, kernel, epsilon, order, support)) + sum(cc * .rbf_poly(Q[k, ], mons))
  }, 0)
  list(prediction = pred, weights = w, poly_coef = cc, system = unname(S), rhs = rhs)
}

#' @rdname RadialBasis
#' @export
RbfLoocv <- function(X, y, epsilons, kernel = "gaussian", degree = -1, smoothing = 0) {
  n <- nrow(as.matrix(X))
  rms <- vapply(epsilons, function(e) {
    f <- RbfInterpolate(X, y, matrix(0, 0, ncol(as.matrix(X))), kernel, e, degree, smoothing)
    Ai <- solve(f$system)
    a <- as.vector(Ai %*% f$rhs)[seq_len(n)]
    sqrt(mean((a / diag(Ai)[seq_len(n)])^2))
  }, 0)
  list(epsilon = epsilons, rmse = rms, best = epsilons[which.min(rms)])
}

#' @rdname RadialBasis
#' @export
RbfMultiscale <- function(X, y, Xnew, supports, degree = -1) {
  P <- as.matrix(X)
  Q <- as.matrix(Xnew)
  res <- y
  pred <- numeric(nrow(Q))
  trail <- numeric(0)
  for (s in supports) {
    f <- RbfInterpolate(P, res, rbind(P, Q), kernel = "wendland", support = s, degree = degree)
    res <- res - f$prediction[seq_len(nrow(P))]
    pred <- pred + f$prediction[nrow(P) + seq_len(nrow(Q))]
    trail <- c(trail, sqrt(mean(res^2)))
  }
  list(prediction = pred, residual_rms = trail)
}

#' @rdname RadialBasis
#' @export
RbfGrid <- function(X, y, xs, ys, ...) {
  pts <- as.matrix(expand.grid(x = xs, y = ys))
  f <- RbfInterpolate(X, y, pts, ...)
  list(surface = matrix(f$prediction, length(ys), length(xs), byrow = TRUE), x = xs, y = ys)
}
