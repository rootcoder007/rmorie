% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.fdtrapz}
\alias{.fdtrapz}
\title{.fdtrapz}
\usage{
.fdtrapz(t, v)
}
\arguments{
\item{t}{A vector; its length is taken and its elements indexed.}
\item{v}{A vector; indexed elementwise.}
}
\description{
A step of the helpers_fda implementation. Called by \code{Scfd}. See the file header for the source the module follows.
}
\value{
The value of \code{s}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.fdtrapz(V, V)
}
