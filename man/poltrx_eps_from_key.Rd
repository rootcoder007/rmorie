% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{poltrx_eps_from_key}
\alias{poltrx_eps_from_key}
\title{Convert a list key of the form "(0,1,1)" back to an integer vector}
\usage{
poltrx_eps_from_key(key)
}
\arguments{
\item{key}{Compared against \code{"()"}.}
}
\description{
A step of the poltrx_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{as.integer}.
}
\keyword{internal}
\examples{
poltrx_eps_from_key(key = 5L)
}
