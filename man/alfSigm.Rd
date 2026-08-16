% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{alfSigm}
\alias{alfSigm}
\title{alfSigm}
\usage{
alfSigm(x)
}
\arguments{
\item{x}{Numeric; passed to \code{exp}.}
}
\description{
A step of the alf_core implementation. Called by \code{Alfmsaat}, \code{Alftriat}, \code{Alftrimu}. See the file header for the source the module follows.
}
\value{
The value of \code{ifelse}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
alfSigm(V)
}
