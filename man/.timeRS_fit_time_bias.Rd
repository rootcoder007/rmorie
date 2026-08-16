% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.timeRS_fit_time_bias}
\alias{.timeRS_fit_time_bias}
\title{.timeRS_fit_time_bias}
\usage{
.timeRS_fit_time_bias(ratings, n_users, n_items, bin_days = 70, n_bins = 30, epochs = 40, lr = 0.005, reg = 0.02, beta = .timeRS_BETA)
}
\arguments{
\item{ratings}{A matrix; indexed by row and column.}
\item{n_users}{Coerced to integer by the body, with \code{as.integer}.}
\item{n_items}{Coerced to integer by the body, with \code{as.integer}.}
\item{bin_days}{Passed to \code{.timeRS_time_bin}. Defaults to \code{70}.}
\item{n_bins}{Passed to \code{.timeRS_time_bin}. Defaults to \code{30}.}
\item{epochs}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{40}.}
\item{lr}{Numeric; combined arithmetically in the body. Defaults to \code{0.005}.}
\item{reg}{Numeric; combined arithmetically in the body. Defaults to \code{0.02}.}
\item{beta}{Passed to \code{.timeRS_deviation}. Defaults to \code{.timeRS_BETA}.}
}
\description{
A step of the timeRS_native implementation. Called by \code{morie_timeRS}, \code{morie_timeRS_fit_time_bias}, \code{morie_timeRS_timesvd} and 1 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{rmse}, \code{rmse_history}, \code{mu}, \code{b_user}, \code{alpha_user}, \code{b_item}, \code{item_bins}, \code{t_user}, \code{beta}, \code{n_instances}, \code{method}, \code{note}.
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
