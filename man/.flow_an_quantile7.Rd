% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.flow_an_quantile7}
\alias{.flow_an_quantile7}
\title{.flow_an_quantile7}
\usage{
.flow_an_quantile7(sorted_x, q)
}
\arguments{
\item{sorted_x}{A vector; its length is taken and its elements indexed.}
\item{q}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the flow_an_native implementation. Called by \code{.anomaly_score}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
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
