% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.state_key}
\alias{.state_key}
\title{.state_key}
\usage{
.state_key(s)
}
\arguments{
\item{s}{A vector; its length is taken.}
}
\description{
A step of the airl_native implementation. Called by \code{morie_soft_value_iteration}. See the file header for the source the module follows.
}
\value{
A character value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.state_key(V)
}
