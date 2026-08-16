% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_spx_trace}
\alias{.morie_spx_trace}
\title{.morie_spx_trace}
\usage{
.morie_spx_trace(A)
}
\arguments{
\item{A}{A matrix; passed to \code{diag}.}
}
\description{
A step of the sp_fill implementation. Called by \code{MantelZ}, \code{MoranRes}. See the file header for the source the module follows.
}
\value{
The value of \code{.morie_fsum}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_spx_trace(V)
}
