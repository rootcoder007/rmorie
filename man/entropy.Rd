% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{entropy}
\alias{entropy}
\title{entropy}
\usage{
entropy(q)
}
\arguments{
\item{q}{Numeric; passed to \code{sum}.}
}
\description{
A step of the prgrl_native implementation. Called by \code{is_curriculum}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
entropy(V)
}
