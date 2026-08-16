% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.t4_vec}
\alias{.t4_vec}
\title{.t4_vec}
\usage{
.t4_vec(x)
}
\arguments{
\item{x}{See Usage.}
}
\description{
A step of the tail4_core implementation. Called by \code{Beggtest}, \code{Boxpierce}, \code{Dprime} and 11 others in the module. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.t4_vec(V)
}
