% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.anomaly_score}
\alias{.anomaly_score}
\title{.anomaly_score}
\usage{
.anomaly_score(X, layers, threshold_quantile = 0.95, reference = NULL)
}
\arguments{
\item{X}{Passed to \code{.flow_an_to_mat}.}
\item{layers}{Passed to \code{.log_prob}.}
\item{threshold_quantile}{Defaults to \code{0.95}.}
\item{reference}{Optional; may be \code{NULL}. Passed to \code{.flow_an_to_mat}.}
}
\description{
A step of the flow_an_native implementation. Called by \code{morie_flow_an}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{score}, \code{threshold}, \code{flag}, \code{n_flagged}, \code{n}, \code{quantile}, \code{self_referenced}, \code{log_likelihood}, \code{method}.
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
