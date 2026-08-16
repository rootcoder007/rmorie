% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.coupling_inverse}
\alias{.coupling_inverse}
\title{.coupling_inverse}
\usage{
.coupling_inverse(y, mask, Ws, bs, Wt, bt, scale_cap = 5)
}
\arguments{
\item{y}{Numeric; combined arithmetically in the body.}
\item{mask}{Numeric; combined arithmetically in the body.}
\item{Ws}{Passed to \code{.st}.}
\item{bs}{Passed to \code{.st}.}
\item{Wt}{Passed to \code{.st}.}
\item{bt}{Passed to \code{.st}.}
\item{scale_cap}{Passed to \code{.st}. Defaults to \code{5}.}
}
\description{
A step of the flow_an_native implementation. Called by \code{.flow_inverse}. See the file header for the source the module follows.
}
\value{
A list with \code{x}, \code{logdet}.
}
\references{
Dinh, L., Sohl-Dickstein, J. & Bengio, S. (2017) "Density Estimation
using Real NVP", ICLR, arXiv:1605.08803. The affine coupling layer,
its Jacobian, and the multi-scale architecture.
Dinh, L., Krueger, D. & Bengio, Y. (2015) "NICE: Non-linear
Independent Components Estimation", ICLR Workshop, arXiv:1410.8516.
The additive coupling RealNVP generalises.
Rezende, D. J. & Mohamed, S. (2015) "Variational Inference with
Normalizing Flows", ICML, PMLR 37, 1530-1538, arXiv:1505.05770.
Papamakarios, G. et al. (2021) "Normalizing Flows for Probabilistic
Modeling and Inference", JMLR 22(57), 1-64, arXiv:1912.02762.
}
\keyword{internal}
