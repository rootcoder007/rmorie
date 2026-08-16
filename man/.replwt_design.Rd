% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.replwt_design}
\alias{.replwt_design}
\title{.replwt_design}
\usage{
.replwt_design(weights, strata = NULL, psu = NULL)
}
\arguments{
\item{weights}{See Usage.}
\item{strata}{Defaults to \code{NULL}.}
\item{psu}{Defaults to \code{NULL}.}
}
\description{
A step of the replwt_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{weights}, \code{strata}, \code{psu}, \code{n}, \code{psu_order}, \code{psu_units}, \code{stratum_psus}, \code{stratum_order}.
}
\references{
----------
Wolter, K. M. (2007) Introduction to Variance Estimation, 2nd
edition, Springer, ISBN 978-0-387-32917-8,
doi:10.1007/978-0-387-35099-8. Ch. 2 (random groups), Ch. 3
(balanced half-samples, the Hadamard construction, Fay\'s
modification), Ch. 4 (the jackknife, JK1 and JKn multipliers) and
Ch. 5 (the bootstrap); the exact equivalence of full-orthogonal
BRR to the stratified variance estimator for linear statistics.

McCarthy, P. J. (1969) Pseudo-replication: half samples, Review
of the International Statistical Institute 37(3), 239-264,
doi:10.2307/1402116.

Rao, J. N. K. & Wu, C. F. J. (1988) Resampling inference with
complex survey data, Journal of the American Statistical
Association 83(401), 231-241, doi:10.1080/01621459.1988.10478591.
}
\keyword{internal}
