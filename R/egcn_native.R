# E(n)-equivariant graph convolution -- re-export of egnnL.
# "egcn" and "egnnL" are two ledger rows citing the same paper
# (Satorras, Hoogeboom & Welling 2021). They are kept as one
# implementation with a re-export so the two entries cannot drift
# apart.
#
# Sources: Satorras, V. G., Hoogeboom, E. & Welling, M. (2021)
# "E(n) Equivariant Graph Neural Networks", *Proceedings of the 38th
# International Conference on Machine Learning (ICML 2021)*, PMLR
# 139, 9323-9332, arXiv:2102.09844. See egnnL_native.R for the
# equations, the equivariance argument and the full references.
#
# Native implementation mirroring Python morie.fn.egcn exactly: the
# same re-exports (edge_message, coord_update, egcl, run_egnn,
# equivariance_error, cheatsheet) plus the same compact aliases
# (equivariantgraphconv -> run_egnn, e_gcn -> run_egnn). The actual
# EGCL implementation lives in egnnL_native.R; this file just
# forwards to it.

# compact alias per ledger/NAMING.md
#' Compact alias per ledger/NAMING.md
#'
#' A step of the egcn_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param H Passed to \code{run_egnn}.
#' @param X Passed to \code{run_egnn}.
#' @param layers Passed to \code{run_egnn}.
#' @param phi_e Passed to \code{run_egnn}.
#' @param phi_x Passed to \code{run_egnn}.
#' @param phi_h Passed to \code{run_egnn}.
#' @param A Passed to \code{run_egnn}.
#' @param C Passed to \code{run_egnn}.
#' @return The value of \code{run_egnn}.
#' @export
#' @examples
#' set.seed(1)
#' r <- equivariantgraphconv(H = rnorm(10), X = rnorm(10), layers = rnorm(10), phi_e = rnorm(10),
#'   phi_x = rnorm(10), phi_h = rnorm(10))
#' TRUE
equivariantgraphconv <- function(H, X, layers, phi_e, phi_x, phi_h,
                                A = NULL, C = NULL) {
  run_egnn(H, X, layers, phi_e, phi_x, phi_h, A, C)
}

# public name resolved by fn/_lazy_map.json
#' Public name resolved by fn/_lazy_map.json
#'
#' A step of the egcn_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param H Passed to \code{run_egnn}.
#' @param X Passed to \code{run_egnn}.
#' @param layers Passed to \code{run_egnn}.
#' @param phi_e Passed to \code{run_egnn}.
#' @param phi_x Passed to \code{run_egnn}.
#' @param phi_h Passed to \code{run_egnn}.
#' @param A Passed to \code{run_egnn}.
#' @param C Passed to \code{run_egnn}.
#' @return The value of \code{run_egnn}.
#' @export
#' @examples
#' set.seed(1)
#' H <- lapply(1:4, function(i) rnorm(2))
#' X <- lapply(1:4, function(i) rnorm(3))
#' phi_e <- function(hi, hj, d2, a) c(hi + hj, d2)
#' phi_x <- function(m) sum(m) * 0.01
#' phi_h <- function(hi, agg) hi + 0.1 * agg[seq_along(hi)]
#' r <- e_gcn(H, X, layers = 2L, phi_e, phi_x, phi_h)
#' str(r, max.level = 1)
e_gcn <- function(H, X, layers, phi_e, phi_x, phi_h, A = NULL,
                  C = NULL) {
  run_egnn(H, X, layers, phi_e, phi_x, phi_h, A, C)
}

# morie entry point: matches the Python payload keys
#' Morie entry point: matches the Python payload keys
#'
#' A step of the egcn_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param H Carried through into a list the body builds.
#' @param X Carried through into a list the body builds.
#' @param layers Coerced to integer by the body, with \code{as.integer}.
#' @param phi_e Passed to \code{run_egnn}.
#' @param phi_x Passed to \code{run_egnn}.
#' @param phi_h Passed to \code{run_egnn}.
#' @param A Passed to \code{run_egnn}.
#' @param C Passed to \code{run_egnn}.
#' @return A list with \code{estimate}, \code{H}, \code{X}, \code{layers}, \code{method},
#' \code{note}.
#' @export
#' @examples
#' set.seed(1)
#' H <- lapply(1:4, function(i) rnorm(2))
#' X <- lapply(1:4, function(i) rnorm(3))
#' phi_e <- function(hi, hj, d2, a) c(hi + hj, d2)
#' phi_x <- function(m) sum(m) * 0.01
#' phi_h <- function(hi, agg) hi + 0.1 * agg[seq_along(hi)]
#' r <- morie_egcn(H, X, layers = 2L, phi_e, phi_x, phi_h)
#' str(r, max.level = 1)
morie_egcn <- function(H, X, layers, phi_e, phi_x, phi_h, A = NULL,
                       C = NULL) {
  list(estimate = run_egnn(H, X, layers, phi_e, phi_x, phi_h, A, C),
       H = H, X = X, layers = as.integer(layers),
       method = "EGNN; Satorras, Hoogeboom & Welling (2021) eqs. (3)-(6)",
       note = paste0("h is E(n) INVARIANT, x is E(n) EQUIVARIANT; ",
                     "see egnnL_native.R for the layer implementation"))
}
