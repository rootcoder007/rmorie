#' Utility maximisation under constraints and the VCG mechanism
#'
#' \code{QuadraticUtilityLagrange}: maximiser of a weighted Euclidean
#' utility under linear equality constraints, with Lagrange multipliers.
#' \code{QuadraticUtilityConstrained}: maximiser on a polyhedron by
#' enumerating KKT active sets. \code{VcgMechanism}: efficient choice and
#' Clarke pivot payments. Identical to the Python arm \code{morie.fn.utilmax}.
#'
#' @param ideal Ideal point.
#' @param W Positive-definite salience matrix.
#' @param C,d Equality constraints C x = d.
#' @param A,b Inequality constraints A x <= b.
#' @param tol Feasibility tolerance.
#' @param values Matrix of values (players by alternatives).
#' @return List.
#' @references Kuhn, H. W. and Tucker, A. W. (1951). Nonlinear programming.
#'   Proceedings of the Second Berkeley Symposium, 481-492.
#'
#'   Clarke, E. H. (1971). Multipart pricing of public goods. Public Choice 11,
#'   17-33.
#'
#'   Groves, T. (1973). Incentives in teams. Econometrica 41, 617-631.
#' @examples
#' QuadraticUtilityLagrange(c(2, 1), diag(2), matrix(c(1, 1), 1), 1)$x
#' VcgMechanism(rbind(c(5, 0), c(0, 3), c(0, 4)))$payments
#' @export
QuadraticUtilityLagrange <- function(ideal, W, C, d) {
  o <- .um_eq(ideal, as.matrix(W), matrix(C, ncol = length(ideal)), d)
  list(x = o$x, multipliers = o$lambda, utility = .um_u(o$x, ideal, as.matrix(W)))
}

.um_eq <- function(xs, W, C, d) {
  if (nrow(C) == 0) return(list(x = xs, lambda = numeric(0)))
  Wi <- solve(W)
  CWi <- C %*% Wi
  M <- CWi %*% t(C)
  r <- as.vector(C %*% xs) - d
  mu <- as.vector(solve(M, r))
  list(x = xs - as.vector(t(CWi) %*% mu), lambda = 2 * mu)
}

.um_u <- function(x, xs, W) -sum((x - xs) * as.vector(W %*% (x - xs)))

#' @rdname QuadraticUtilityLagrange
#' @export
QuadraticUtilityConstrained <- function(ideal, W, A, b, tol = 1e-10) {
  W <- as.matrix(W)
  A <- matrix(A, ncol = length(ideal))
  m <- nrow(A)
  for (r in 0:min(m, length(ideal))) {
    sets <- if (r == 0) list(integer(0)) else utils::combn(m, r, simplify = FALSE)
    for (S in sets) {
      o <- tryCatch(.um_eq(ideal, W, A[S, , drop = FALSE], b[S]), error = function(e) NULL)
      if (is.null(o)) next
      if (all(as.vector(A %*% o$x) <= b + tol) && all(o$lambda >= -tol)) {
        mult <- numeric(m)
        mult[S] <- o$lambda
        return(list(x = o$x, multipliers = mult, active = S - 1L, utility = .um_u(o$x, ideal, W)))
      }
    }
  }
  stop("no feasible KKT point: the constraints may be inconsistent")
}

#' @rdname QuadraticUtilityLagrange
#' @export
VcgMechanism <- function(values) {
  V <- as.matrix(values)
  tot <- colSums(V)
  ch <- which.max(tot)
  pay <- vapply(seq_len(nrow(V)), function(i) {
    others <- tot - V[i, ]
    max(others) - others[ch]
  }, 0)
  list(choice = ch - 1L, payments = pay, utilities = V[, ch] - pay, welfare = unname(tot[ch]))
}
