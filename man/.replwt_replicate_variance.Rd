% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.replwt_replicate_variance}
\alias{.replwt_replicate_variance}
\title{.replwt_replicate_variance}
\usage{
.replwt_replicate_variance(estimator, d, rep, values = NULL)
}
\arguments{
\item{estimator}{See Usage.}
\item{d}{See Usage.}
\item{rep}{See Usage.}
\item{values}{Defaults to \code{NULL}.}
}
\description{
Part of the replwt_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{theta}, \code{variance}, \code{std_error}, \code{replicates}, \code{n_replicates}, \code{method}.
}
\references{
----------
Wolter, K. M. (2007) Introduction to Variance Estimation, 2nd
edition, Springer, ISBN 978-0-387-32917-8,
doi:10.1007/978-0-387-35099-8. Ch. 2 (random groups), Ch. 3
(balanced half-samples, the Hadamard construction, Fay\\'s
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
