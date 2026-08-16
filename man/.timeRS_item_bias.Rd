% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.timeRS_item_bias}
\alias{.timeRS_item_bias}
\title{.timeRS_item_bias}
\usage{
.timeRS_item_bias(b_i, bins, t, bin_days = 70, n_bins = 30)
}
\arguments{
\item{b_i}{See Usage.}
\item{bins}{See Usage.}
\item{t}{See Usage.}
\item{bin_days}{Defaults to \code{70}.}
\item{n_bins}{Defaults to \code{30}.}
}
\description{
Part of the timeRS_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{bias}, \code{bin}.
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
dev_u(t) = sign(t - t_u) |t - t_u|^beta with t_u the user\\'s mean
rating date and beta = 0.4 set by cross-validation; the resulting
time-dependent user bias; item bias in time bins; and the per-day user
term.

Koren, Y. (2009) "Collaborative Filtering with Temporal Dynamics",
*KDD \\'09*, 447-456, doi:10.1145/1557019.1557072. The full conference
treatment, including the spline alternative to the linear drift.

Koren, Y. (2008) "Factorization Meets the Neighborhood", *KDD \\'08*,
426-434, doi:10.1145/1401890.1401944. The SVD++ base this extends;
implemented in :mod:`svdpp`.
}
\keyword{internal}
