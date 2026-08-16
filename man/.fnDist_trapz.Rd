% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.fnDist_trapz}
\alias{.fnDist_trapz}
\title{.fnDist_trapz}
\usage{
.fnDist_trapz(t, v)
}
\arguments{
\item{t}{A vector; its length is taken and its elements indexed.}
\item{v}{A vector; indexed elementwise.}
}
\description{
A step of the fnDist implementation. Called by \code{FnDist}. See the file header for the source the module follows.
}
\value{
The value of \code{s}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.fnDist_trapz(V, V)
}
