% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.groebn_sub}
\alias{.groebn_sub}
\title{.groebn_sub}
\usage{
.groebn_sub(f, g)
}
\arguments{
\item{f}{Passed to \code{.groebn_add}.}
\item{g}{Iterated over elementwise, with \code{lapply}.}
}
\description{
A step of the groebn_native implementation. Called by \code{.groebn_divide}, \code{.groebn_spoly}. See the file header for the source the module follows.
}
\value{
The value of \code{.groebn_add}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.groebn_sub(V, V)
}
