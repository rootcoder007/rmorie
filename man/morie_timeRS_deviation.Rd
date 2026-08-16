% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_timeRS_deviation}
\alias{morie_timeRS_deviation}
\title{Exported helpers}
\usage{
morie_timeRS_deviation(t, t_user, beta = .timeRS_BETA)
}
\arguments{
\item{t}{Passed to \code{.timeRS_deviation}.}
\item{t_user}{Passed to \code{.timeRS_deviation}.}
\item{beta}{Passed to \code{.timeRS_deviation}. Defaults to \code{.timeRS_BETA}.}
}
\description{
A step of the timeRS_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{.timeRS_deviation}.
}
\references{
----------
Koren, Y. (2010) "Collaborative filtering with temporal dynamics",
*Communications of the ACM* 53(4), 89-97,
doi:10.1145/1721654.1721677. [PDF supplied by Vee.] The two measured
effects in the Netflix data -- the abrupt shift of rating scale in
early 2004 from around 3.4 to above 3.6 stars, and ratings increasing
with movie age; the argument that this differs from concept drift
because many characteristics shift simultaneously and that classical
time-window or instance-decay approaches cannot work as they lose too
many signals when discarding data instances; the time deviation
dev_u(t) = sign(t - t_u) |t - t_u|^beta with t_u the user\'s mean
rating date and beta = 0.4 set by cross-validation; the resulting
time-dependent user bias; item bias in time bins; and the per-day user
term.

Koren, Y. (2009) "Collaborative Filtering with Temporal Dynamics",
*KDD \'09*, 447-456, doi:10.1145/1557019.1557072. The full conference
treatment, including the spline alternative to the linear drift.

Koren, Y. (2008) "Factorization Meets the Neighborhood", *KDD \'08*,
426-434, doi:10.1145/1401890.1401944. The SVD++ base this extends;
implemented in :mod:`svdpp`.
}
\keyword{internal}
\examples{
morie_timeRS_deviation(t = 5L, t_user = 5L)
}
