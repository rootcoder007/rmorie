% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.stahdo_subsample_dirs}
\alias{.stahdo_subsample_dirs}
\title{.stahdo_subsample_dirs}
\usage{
.stahdo_subsample_dirs(M, n, p, n_dirs, seed)
}
\arguments{
\item{M}{A matrix; indexed by row and column.}
\item{n}{Numeric; combined arithmetically in the body.}
\item{p}{A count; the body uses it as \code{seq_len(...)}.}
\item{n_dirs}{Coerced to integer by the body, with \code{as.integer}.}
\item{seed}{Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the stahdo_native implementation. Called by \code{.stahdo_outlyingness}. See the file header for the source the module follows.
}
\value{
A list with \code{dirs}, \code{exhaustive}.
}
\references{
----------
Maronna, R. A. & Yohai, V. J. (1995) "The behavior of the
Stahel-Donoho robust multivariate estimator", *Journal of the
American Statistical Association* 90(429), 330-341,
doi:10.1080/01621459.1995.10476517.
}
\keyword{internal}
