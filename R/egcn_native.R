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
#' @return A list with \code{estimate}, \code{H}, \code{X}, \code{layers}, \code{method}, \code{note}.
#' @export
morie_egcn <- function(H, X, layers, phi_e, phi_x, phi_h, A = NULL,
                       C = NULL) {
  list(estimate = run_egnn(H, X, layers, phi_e, phi_x, phi_h, A, C),
       H = H, X = X, layers = as.integer(layers),
       method = "EGNN; Satorras, Hoogeboom & Welling (2021) eqs. (3)-(6)",
       note = paste0("h is E(n) INVARIANT, x is E(n) EQUIVARIANT; ",
                     "see egnnL_native.R for the layer implementation"))
}
