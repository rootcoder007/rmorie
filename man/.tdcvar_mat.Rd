% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tdcvar_mat}
\alias{.tdcvar_mat}
\title{.tdcvar_mat}
\usage{
.tdcvar_mat(X)
}
\arguments{
\item{X}{Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.}
}
\description{
A step of the tdcvar_native implementation. Called by \code{.tdcvar_ip_weights_history}, \code{morie_tdcvar}. See the file header for the source the module follows.
}
\value{
The value of \code{M}, as built in the body.
}
\references{
----------
Hernan, M. A. & Robins, J. M. (2020) Causal Inference: What If, Boca
Raton: Chapman & Hall/CRC. Ch. 20 (treatment-confounder feedback),
Sec. 21.2 (IP weighting for time-varying treatments), Sec. 12.3 (the
stabilized weights and the mean-1 diagnostic).

Robins, J. (1986) "A new approach to causal inference in mortality
studies with a sustained exposure period", Mathematical Modelling
7(9-12), 1393-1512, doi:10.1016/0270-0255(86)90088-6 -- where the
problem and the weighted solution were first set out.
}
\keyword{internal}
