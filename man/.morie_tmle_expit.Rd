% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_tmle_expit}
\alias{.morie_tmle_expit}
\title{.morie_tmle_expit}
\usage{
.morie_tmle_expit(x)
}
\arguments{
\item{x}{Passed to \code{pmax}.}
}
\description{
A step of the tmle_native implementation. Called by \code{morie_tmle_ate}, \code{morie_tmle_time_varying}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_tmle_expit(V)
}
