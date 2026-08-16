% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.replwt_bootstrap_weights}
\alias{.replwt_bootstrap_weights}
\title{.replwt_bootstrap_weights}
\usage{
.replwt_bootstrap_weights(d, R = 200, seed = 1)
}
\arguments{
\item{d}{A list; the body reads \code{$psu_units}, \code{$stratum_order}, \code{$stratum_psus}, \code{$weights} from it.}
\item{R}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{200}.}
\item{seed}{Defaults to \code{1}.}
}
\description{
A step of the replwt_native implementation. Called by \code{morie_replwt}. See the file header for the source the module follows.
}
\value{
A list with \code{weights}, \code{scale}, \code{n_replicates}, \code{seed}, \code{method}.
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
