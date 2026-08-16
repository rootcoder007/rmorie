% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s03mean}
\alias{.s03mean}
\title{.s03mean}
\usage{
.s03mean(v)
}
\arguments{
\item{v}{A vector; its length is taken.}
}
\description{
A step of the helpers_s03 implementation. Called by \code{.btsieve_arfit}, \code{.s03corr}, \code{.s03var} and 42 others in the module. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.s03mean(V)
}
