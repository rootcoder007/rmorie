# P-splines: B-splines on many knots with a difference penalty.
# Sources: Eilers, P. H. C. & Marx, B. D. (1996) "Flexible Smoothing
# with B-splines and Penalties", Statistical Science 11(2), 89-121,
# doi:10.1214/ss/1038425655 (Sec. 2 for the B-spline basis and the
# Cox-de Boor construction; Sec. 3 for the difference penalty on
# adjacent coefficients and the penalised normal equations
# (B'B + lambda D'D) a = B'y; Sec. 4 for the polynomial limit as
# lambda -> infinity under a d-th order penalty; Sec. 5-6 for the
# effective dimension as the trace of the smoother matrix, the
# leave-one-out cross-validation formula through the hat diagonal,
# and AIC as criteria for choosing lambda); O'Sullivan, F. (1986)
# "A Statistical Perspective on Ill-Posed Inverse Problems",
# Statistical Science 1(4), 502-518, doi:10.1214/ss/1177013525, for
# the penalised-B-spline idea that Eilers and Marx simplify by
# penalising coefficient differences.
#
# Native implementation mirroring Python morie.fn.smfd.fit exactly:
# the same Cox-de Boor recursion for the B-spline basis on the same
# evenly-spaced extended knot sequence, the same d-th order
# difference matrix built by iterated first differences, and the
# same penalised normal equations (B'B + lambda D'D) a = B'y. The
# Python arm draws no random numbers, so the shared generator is
# not touched here.

# Internal: evenly spaced knots, extended by 'degree' at each end.
# Mirrors Python knot_sequence(xmin, xmax, nseg, degree) in smfd.
#' Internal: evenly spaced knots, extended by \'degree\' at each end
#'
#' Mirrors Python knot_sequence(xmin, xmax, nseg, degree) in smfd.
#'
#' @param xmin Numeric; combined arithmetically in the body.
#' @param xmax Numeric; combined arithmetically in the body.
#' @param nseg Numeric; combined arithmetically in the body. Defaults to \code{10L}.
#' @param degree Numeric; combined arithmetically in the body. Defaults to \code{3L}.
#' @return A numeric value.
#' @export
smfd_knot_sequence <- function(xmin, xmax, nseg = 10L, degree = 3L) {
  nseg   <- as.integer(nseg)
  degree <- as.integer(degree)
  if (nseg < 1L)  stop("smfd: nseg must be at least 1")
  if (degree < 0L) stop("smfd: degree cannot be negative")
  if (!(xmax > xmin)) stop("smfd: xmax must exceed xmin")
  h <- (xmax - xmin) / nseg
  xmin + h * (seq_len(nseg + 2L * degree + 1L) - 1L - degree)
}

# Internal: Cox-de Boor recursion for one B-spline. k is 1-based
# (Python k-1 in 0-based). Mirrors Python .bspline(x, k, degree,
# knots) in smfd, including the half-open support, the special
# right-closed last interval, and the strict-positive denominators.
#' Internal: Cox-de Boor recursion for one B-spline. k is 1-based
#'
#' (Python k-1 in 0-based). Mirrors Python .bspline(x, k, degree, knots)
#' in smfd, including the half-open support, the special right-closed
#' last interval, and the strict-positive denominators.
#'
#' @param x Numeric; combined arithmetically in the body.
#' @param k Numeric; combined arithmetically in the body.
#' @param degree Numeric; combined arithmetically in the body.
#' @param knots A vector; its length is taken and its elements indexed.
#' @return The value of \code{out}, as built in the body.
#' @export
smfd_bspline_one <- function(x, k, degree, knots) {
  if (degree == 0L) {
    last <- k == length(knots) - 1L
    if ((knots[k] <= x && x < knots[k + 1L]) ||
        (last && x == knots[k + 1L])) return(1.0)
    return(0.0)
  }
  out <- 0.0
  d1 <- knots[k + degree] - knots[k]
  if (d1 > 0)
    out <- out + (x - knots[k]) / d1 *
             smfd_bspline_one(x, k, degree - 1L, knots)
  d2 <- knots[k + degree + 1L] - knots[k + 1L]
  if (d2 > 0)
    out <- out + (knots[k + degree + 1L] - x) / d2 *
             smfd_bspline_one(x, k + 1L, degree - 1L, knots)
  out
}

# Internal: the n x p B-spline design matrix. Mirrors Python
# bspline_basis(x, knots, degree) in smfd.
#' Internal: the n x p B-spline design matrix. Mirrors Python
#'
#' bspline_basis(x, knots, degree) in smfd.
#'
#' @param x A vector; its length is taken and its elements indexed.
#' @param knots A vector; its length is taken.
#' @param degree Numeric; combined arithmetically in the body. Defaults to \code{3L}.
#' @return The value of \code{B}, as built in the body.
#' @export
smfd_bspline_basis <- function(x, knots, degree = 3L) {
  degree <- as.integer(degree)
  p <- length(knots) - degree - 1L
  if (p < 1L)
    stop(sprintf("smfd: the knot sequence is too short for degree %d",
                 degree))
  n <- length(x)
  B <- matrix(0, nrow = n, ncol = p)
  for (k in seq_len(p))
    for (i in seq_len(n))
      B[i, k] <- smfd_bspline_one(x[i], k, degree, knots)
  B
}

# Internal: the d-th order difference matrix, built by iterated
# first differences starting from the identity. Mirrors Python
# difference_matrix(p, order) in smfd.
#' Internal: the d-th order difference matrix, built by iterated
#'
#' first differences starting from the identity. Mirrors Python
#' difference_matrix(p, order) in smfd.
#'
#' @param p A matrix; passed to \code{diag}.
#' @param order A count; the body uses it as \code{seq_len(...)}. Defaults to \code{2L}.
#' @return The value of \code{D}, as built in the body.
#' @export
smfd_difference_matrix <- function(p, order = 2L) {
  p     <- as.integer(p)
  order <- as.integer(order)
  if (order < 0L) stop("smfd: the penalty order cannot be negative")
  if (order >= p)
    stop(sprintf("smfd: a %d-th order penalty needs more than %d coefficients", order, p))
  D <- diag(p)
  for (i in seq_len(order))
    D <- D[-1L, , drop = FALSE] - D[-nrow(D), , drop = FALSE]
  D
}

#' P-spline fit
#'
#' Penalised least squares on a B-spline basis with a difference
#' penalty on the coefficients (Eilers & Marx 1996). The normal
#' equations are \code{(B'B + lambda D'D) a = B'y} with \code{B}
#' the B-spline design matrix and \code{D} the \code{order}-th
#' difference matrix. The penalty order fixes the limit: as
#' \code{lam} tends to infinity the fit becomes a polynomial of
#' degree \code{order - 1} exactly, with \code{order = 2} giving
#' the OLS line. The effective dimension is the trace of the
#' smoother matrix \code{H = B (B'B + lambda D'D)^{-1} B'}, and
#' the leave-one-out deletion residual is \code{e_i / (1 - h_ii)}
#' from the hat diagonal.
#'
#' @param x Predictor values.
#' @param y Response values, same length as \code{x}.
#' @param nseg Number of interior segments; the total number of
#'   knots is \code{nseg + 2 * degree + 1}.
#' @param degree B-spline degree (0 gives piecewise constant).
#' @param lam Non-negative penalty weight.
#' @param order Penalty order; the limit as \code{lam -> Inf} is
#'   the polynomials of degree \code{order - 1}.
#' @param weights Optional non-negative observation weights.
#' @return A list with \code{estimate} (the effective dimension),
#'   \code{coefficients}, \code{fitted}, \code{residuals},
#'   \code{rss}, \code{hat_diagonal}, \code{effective_dimension},
#'   \code{knots}, \code{degree}, \code{nseg}, \code{order},
#'   \code{lam}, \code{n}, \code{p}, \code{sigma2}, \code{method}.
#' @references Eilers, P. H. C. & Marx, B. D. (1996). Flexible
#'   smoothing with B-splines and penalties. Statistical Science,
#'   11(2), 89-121.
#' @export
morie_smfd <- function(x, y, nseg = 10L, degree = 3L, lam = 1.0,
                       order = 2L, weights = NULL) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  n <- length(x)
  if (n != length(y)) stop("smfd: x and y must have the same length")
  if (n < 2)         stop("smfd: need at least two points")
  lam   <- as.numeric(lam)
  if (lam < 0) stop("smfd: lambda cannot be negative")
  nseg   <- as.integer(nseg)
  degree <- as.integer(degree)
  order  <- as.integer(order)
  knots <- smfd_knot_sequence(min(x), max(x), nseg, degree)
  B     <- smfd_bspline_basis(x, knots, degree)
  p     <- ncol(B)
  D     <- smfd_difference_matrix(p, order)
  if (is.null(weights)) w <- rep(1.0, n) else w <- as.numeric(weights)
  # A = B' diag(w) B + lam D' D, b = B' (w .* y); same operands
  # and order as the Python arm so solve() agrees to machine
  # precision (both call LAPACK dgesv).
  A <- crossprod(B, w * B) + lam * crossprod(D)
  b <- as.numeric(crossprod(B, w * y))
  a <- as.numeric(solve(A, b))
  fitted <- as.numeric(B %*% a)
  resid  <- y - fitted
  rss    <- sum(resid * resid)
  Ainv   <- solve(A)
  BAinv  <- B %*% Ainv
  hat    <- rowSums(BAinv * B)            # diag(B A^{-1} B')
  ed     <- sum(hat)
  sigma2 <- rss / max(n - ed, 1e-9)
  list(estimate = ed,
       coefficients = a,
       fitted = fitted,
       residuals = resid,
       rss = rss,
       hat_diagonal = hat,
       effective_dimension = ed,
       knots = knots,
       degree = degree,
       nseg = nseg,
       order = order,
       lam = lam,
       n = n,
       p = p,
       sigma2 = sigma2,
       method = "P-spline: (B'B + lambda D'D) a = B'y; Eilers & Marx (1996) Sec. 3")
}
