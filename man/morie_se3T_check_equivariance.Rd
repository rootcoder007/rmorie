% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_se3T_check_equivariance}
\alias{morie_se3T_check_equivariance}
\title{Check equivariance: rotate and translate the input; the output must}
\usage{
morie_se3T_check_equivariance(positions, type0, type1, layer = NULL, axis = c(0.3, -0.7, 0.4), angle = 1.1, translation = c(2, -1, 0.5), tol = 1e-09)
}
\arguments{
\item{positions}{Passed to \code{.se3T_mat}.}
\item{type0}{Passed to \code{f}.}
\item{type1}{Passed to \code{.se3T_mat}.}
\item{layer}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
\item{axis}{Passed to \code{morie_se3T_rotation_matrix}. Defaults to \code{c(0.3, -0.7, 0.4)}.}
\item{angle}{Passed to \code{morie_se3T_rotation_matrix}. Defaults to \code{1.1}.}
\item{translation}{Passed to \code{.se3T_vec}. Defaults to \code{c(2, -1, 0.5)}.}
\item{tol}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1e-09}.}
}
\description{
follow Part of the se3T_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{type1_deviation}, \code{type0_deviation}, \code{weight_deviation}, \code{equivariant}, \code{weights_invariant}, \code{method}, \code{note}.
}
\references{
----------
Fuchs, F. B., Worrall, D. E., Fischer, V. & Welling, M. (2020)
"SE(3)-Transformers: 3D Roto-Translation Equivariant Attention
Networks", *Advances in Neural Information Processing Systems 33
(NeurIPS 2020)*, 1970-1981, arXiv:2006.10503. The abstract and Sec.
1-3: a variant of the self-attention module for 3D point clouds and
graphs which is equivariant under continuous 3D roto-translations;
that equivariance is important to ensure stable and predictable
performance in the presence of nuisance transformations of the input,
with increased weight-tying as a positive corollary; that
SE(3)-equivariance generalises the translational weight-tying of
conventional convolutions to roto-translations in 3D and restricts the
space of learnable functions to a subspace adhering to the symmetries
of the task; the use of self-attention to operate on point clouds and
graphs with varying numbers of points; and that the model outperforms
both a strong non-equivariant attention baseline and an equivariant
model without attention.

Thomas, N., Smidt, T., Kearnes, S., Yang, L., Li, L., Kohlhoff, K. &
Riley, P. (2018) "Tensor field networks: Rotation- and
translation-equivariant neural networks for 3D point clouds",
arXiv:1802.08219. The equivariant kernel basis.

Vaswani, A. et al. (2017) "Attention Is All You Need", *NIPS 2017*,
5998-6008, arXiv:1706.03762. The attention module being made
equivariant.
}
\keyword{internal}
