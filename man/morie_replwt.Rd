% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_replwt}
\alias{morie_replwt}
\title{morie_replwt}
\usage{
morie_replwt(d, method = "jkn", R = 200, fay = 0, seed = 1)
}
\arguments{
\item{d}{Passed to \code{.replwt_jackknife_weights}.}
\item{method}{One of \code{"brr"}, \code{"fay"}, \code{"jk1"}, \code{"jkn"}. Defaults to \code{"jkn"}.}
\item{R}{Passed to \code{.replwt_bootstrap_weights}. Defaults to \code{200}.}
\item{fay}{A flag; the body branches on it. Defaults to \code{0}.}
\item{seed}{Passed to \code{.replwt_bootstrap_weights}. Defaults to \code{1}.}
}
\description{
A step of the replwt_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{weights}, \code{scale}, \code{n_replicates}, \code{method}, \code{dropped}, \code{hadamard_order}, \code{fay}, \code{seed}.
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
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
morie_replwt(D)
}
