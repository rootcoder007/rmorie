% Generated from the module source by ledger/gen_rd.R -- roxygen2 hangs
% on this package. Usage comes from args(), so it tracks the formals.
\name{morie_egnnL_equivariance_error}
\alias{morie_egnnL_equivariance_error}
\title{E(n)-equivariant graph neural networks}
\usage{
morie_egnnL_equivariance_error(H, X, phi_e, phi_x, phi_h, Q, g, layers = 2, C = NULL)
}
\description{
E(n)-equivariant graph neural networks. Equivariant Graph Neural Networks", *Proceedings of the 38th International Conference on Machine Learning (ICML 2021)*, PMLR 139, 9323-9332, arXiv:2102.09844. Sec. 3 (the EGCL of eqs. (3)-(6), with C = 1/(M-1); the statement that eq. (4) is the main difference from standard GNNs and the reason equivariances 1 and 2 are preserved). Sec. 3.1 (the equivariance condition Qx + g; that m_ij is E(n) invariant because it depends on positions only through squared distances; that the weighted sum of differences transforms as a type-1 vector; and the inductive argument for composed layers). Sec. 3.2 (the momentum variant replacing eq. (4)). Thomas, N., Smidt, T., Kearnes, S., Yang, L., Li, L., Kohlhoff, K. & Translation-Equivariant Neural Networks for 3D Point Clouds", arXiv:1802.08219. The higher-order-representation approach this avoids. Native implementation mirroring Python morie.fn.egnnL exactly: the same four-equation layer (m_ij, x-update, m_i, h-update), the same E(n)-invariance argument, the same C = 1/(n-1) averaging, the same momentum variant (eq. 4 replaced by a velocity update integrated with the same X <- X + dt * Vn step), and the same equivariance error measurement (max abs gap between transforming the input vs transforming the output) with the same 1e-9 tolerance.
}
\value{
A list with components \code{C}, \code{coordinate_error}, \code{feature_error}, \code{equivariant}, \code{invariant}, \code{note}.
}
\references{
Sources: Satorras, V. G., Hoogeboom, E. & Welling, M. (2021) "E(n)
Riley, P. (2018) "Tensor Field Networks: Rotation- and
}
\keyword{internal}
