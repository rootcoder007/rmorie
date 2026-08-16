% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rndnet_running_stats_new}
\alias{.rndnet_running_stats_new}
\title{Private: Welford running mean and variance for the normalisers}
\usage{
.rndnet_running_stats_new(n)
}
\arguments{
\item{n}{A count; the body uses it as \code{rep(...)}.}
}
\description{
A step of the rndnet_native implementation. Called by \code{morie_rndnet}. See the file header for the source the module follows.
}
\value{
The value of \code{e}, as built in the body.
}
\keyword{internal}
\examples{
.rndnet_running_stats_new(n = 5L)
}
