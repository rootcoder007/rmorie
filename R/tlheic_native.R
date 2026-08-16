# Estimating the efficient influence curve numerically.
# Sources: van der Laan, M. J. & Rose, S. (2018) *Targeted
# Learning in Data Science*, Springer, doi:10.1007/978-3-319-65304-4.
# Chap. 8 (a machine-learning based estimator of an efficient
# influence curve which avoids the need for its analytic
# computation; many estimation problems in which the object
# exists only in implicit form and is extremely hard to compute
# from it; the representation of a cadlag function of finite
# variation norm as a sum over subsets of integrals against
# products of indicator basis functions; the restriction to H_M,
# the subset with variation norm below M; and the numerical
# computation of the projection of an initial gradient onto the
# tangent space). Bickel, P. J., Klaassen, C. A. J., Ritov, Y. &
# Wellner, J. A. (1993) *Efficient and Adaptive Estimation for
# Semiparametric Models*, Johns Hopkins University Press. Tangent
# spaces, pathwise differentiability and canonical gradients.
# Carone, M., Diaz, I. & van der Laan, M. J. (2018) "Higher-Order
# Targeted Loss-Based Estimation", in *Targeted Learning in Data
# Science*, Springer, 483-510, doi:10.1007/978-3-319-65304-4_26.
#
# Native implementation mirroring Python morie.fn.tlheic exactly:
# the same central-difference numerical derivative, the same
# inner product, the same coefficient recovery by solving the
# direction system (with a small ridge to keep it well posed),
# the same mean-centring, and the same held-out verification.

#' morie_tlheic
#'
#' Part of the tlheic_native implementation; see the file header for the
#' source it follows.
#'
#' @param psi_of_P Defaults to \code{NULL}.
#' @param basis Defaults to \code{NULL}.
#' @param D Defaults to \code{NULL}.
#' @param score Defaults to \code{NULL}.
#' @param weights Defaults to \code{NULL}.
#' @param h Defaults to \code{1e-05}.
#' @param tol Defaults to \code{1e-04}.
#' @param ridge Defaults to \code{1e-08}.
#' @param mode Defaults to \code{c("estimate", "verify", "grad", "deriv")}.
#' @return The value of \code{estimate_eic}.
#' @export
morie_tlheic <- function(psi_of_P = NULL, basis = NULL, D = NULL,
                         score = NULL, weights = NULL,
                         h = 1e-5, tol = 1e-4, ridge = 1e-8,
                         mode = c("estimate", "verify", "grad",
                                  "deriv")) {
  mode <- match.arg(mode)
  if (mode == "deriv")
    return(numerical_derivative(psi_of_P, weights, score, h = h))
  if (mode == "grad")
    return(gradient_inner_product(D, score, weights))
  if (mode == "verify")
    return(verify_gradient(psi_of_P, D, score, weights,
                           h = h, tol = tol))
  estimate_eic(psi_of_P, basis, weights = weights, h = h,
               ridge = ridge)
}

#' numerical_derivative
#'
#' Part of the tlheic_native implementation; see the file header for the
#' source it follows.
#'
#' @param psi_of_P See Usage.
#' @param weights See Usage.
#' @param score See Usage.
#' @param h Defaults to \code{1e-05}.
#' @return A numeric value.
#' @export
numerical_derivative <- function(psi_of_P, weights, score, h = 1e-5) {
  w <- as.numeric(weights); s <- as.numeric(score)
  if (length(w) != length(s))
    stop(sprintf("tlheic: %d weights but %d score values",
                 length(w), length(s)))
  m <- sum(w * s) / sum(w)
  tilt <- function(eps) {
    v <- w * (1 + eps * (s - m))
    if (any(v <= 0))
      stop("tlheic: the perturbation left the simplex; use a smaller h")
    t <- sum(v); v / t
  }
  (psi_of_P(tilt(h)) - psi_of_P(tilt(-h))) / (2 * h)
}

#' gradient_inner_product
#'
#' Part of the tlheic_native implementation; see the file header for the
#' source it follows.
#'
#' @param D See Usage.
#' @param score See Usage.
#' @param weights Defaults to \code{NULL}.
#' @return A numeric value.
#' @export
gradient_inner_product <- function(D, score, weights = NULL) {
  d <- as.numeric(D); s <- as.numeric(score)
  if (length(d) != length(s))
    stop(sprintf("tlheic: %d gradient values but %d score values",
                 length(d), length(s)))
  w <- if (is.null(weights)) rep(1 / length(d), length(d))
       else as.numeric(weights)
  t <- sum(w)
  sum(w * d * s) / t
}

#' estimate_eic
#'
#' Part of the tlheic_native implementation; see the file header for the
#' source it follows.
#'
#' @param psi_of_P See Usage.
#' @param basis See Usage.
#' @param weights Defaults to \code{NULL}.
#' @param h Defaults to \code{1e-05}.
#' @param ridge Defaults to \code{1e-08}.
#' @return A list with \code{estimate}, \code{D}, \code{coefficients}, \code{n_directions}, \code{mean}, \code{method}, \code{note}.
#' @export
estimate_eic <- function(psi_of_P, basis, weights = NULL, h = 1e-5,
                         ridge = 1e-8) {
  B <- as.matrix(basis)
  n <- nrow(B); p <- ncol(B)
  w <- if (is.null(weights)) rep(1 / n, n) else as.numeric(weights)
  tot <- sum(w); w <- w / tot
  C <- matrix(0, n, p)
  for (j in seq_len(p)) {
    col <- B[, j]
    mj <- sum(w * col)
    C[, j] <- col - mj
  }
  rhs <- vapply(seq_len(p), function(j)
    numerical_derivative(psi_of_P, w, C[, j], h), numeric(1))
  G <- crossprod(C * sqrt(w), C * sqrt(w))
  G <- G + diag(ridge, p)
  coef <- as.numeric(solve(G, rhs))
  D <- as.numeric(C %*% coef)
  m <- sum(w * D)
  D <- D - m
  list(estimate = D, D = D, coefficients = coef,
       n_directions = p, mean = sum(w * D),
       method = "numerical estimation of the efficient influence curve; van der Laan & Rose (2018) Chap. 8",
       note = "no analytic derivation: the gradient is identified by how the parameter MOVES under perturbation")
}

#' verify_gradient
#'
#' Part of the tlheic_native implementation; see the file header for the
#' source it follows.
#'
#' @param psi_of_P See Usage.
#' @param D See Usage.
#' @param score See Usage.
#' @param weights Defaults to \code{NULL}.
#' @param h Defaults to \code{1e-05}.
#' @param tol Defaults to \code{1e-04}.
#' @return A list with \code{derivative}, \code{inner_product}, \code{difference}, \code{verified}, \code{note}.
#' @export
verify_gradient <- function(psi_of_P, D, score, weights = NULL,
                            h = 1e-5, tol = 1e-4) {
  D <- as.numeric(D)
  n <- length(D)
  w <- if (is.null(weights)) rep(1 / n, n) else as.numeric(weights)
  lhs <- numerical_derivative(psi_of_P, w, score, h)
  rhs <- gradient_inner_product(D, score, w)
  list(derivative = lhs, inner_product = rhs,
       difference = abs(lhs - rhs),
       verified = abs(lhs - rhs) < as.numeric(tol),
       note = "must hold along ANY path, including ones not used to fit the gradient")
}

#' .tlheic_cheatsheet
#'
#' Part of the tlheic_native implementation; see the file header for the
#' source it follows.
#'
#' @return A character value.
#' @export
.tlheic_cheatsheet <- function() {
  paste("tlheic: for many parameters the efficient influence curve ",
        "exists only IMPLICITLY and deriving it is what stops the ",
        "method being used. Estimate it from the DEFINITION ",
        "instead: d/d_eps Psi(P_eps) = E[D* s], so perturbing ",
        "along directions and reading off how the parameter moves ",
        "identifies D*. Represent it in the HAL indicator basis ",
        "with a variation-norm bound, and the tangent-space ",
        "projection becomes a numerical regression. Verify on a ",
        "HELD-OUT direction -- the fitted ones satisfy it by ",
        "construction.", sep = "")
}
