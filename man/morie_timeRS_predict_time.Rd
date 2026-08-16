% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_timeRS_predict_time}
\alias{morie_timeRS_predict_time}
\title{morie_timeRS_predict_time}
\usage{
morie_timeRS_predict_time(mu, b_u, alpha_u, t_user, b_i, item_bins, t, p_u = NULL, q_i = NULL, per_day = NULL, bin_days = 70, beta = .timeRS_BETA)
}
\arguments{
\item{mu}{See Usage.}
\item{b_u}{See Usage.}
\item{alpha_u}{See Usage.}
\item{t_user}{See Usage.}
\item{b_i}{See Usage.}
\item{item_bins}{See Usage.}
\item{t}{See Usage.}
\item{p_u}{Defaults to \code{NULL}.}
\item{q_i}{Defaults to \code{NULL}.}
\item{per_day}{Defaults to \code{NULL}.}
\item{bin_days}{Defaults to \code{70}.}
\item{beta}{Defaults to \code{.timeRS_BETA}.}
}
\description{
Part of the timeRS_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{.timeRS_predict_time}.
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
