% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_flow_an}
\alias{morie_flow_an}
\title{morie_flow_an}
\usage{
morie_flow_an(X, layers, threshold_quantile = 0.95, reference = NULL)
}
\arguments{
\item{X}{See Usage.}
\item{layers}{See Usage.}
\item{threshold_quantile}{Defaults to \code{0.95}.}
\item{reference}{Defaults to \code{NULL}.}
}
\description{
Part of the flow_an_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{.anomaly_score}.
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
