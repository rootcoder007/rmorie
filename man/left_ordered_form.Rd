% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{left_ordered_form}
\alias{left_ordered_form}
\title{left_ordered_form}
\usage{
left_ordered_form(Z) 
}
\arguments{
\item{Z}{A matrix; passed to \code{as.matrix}.}
}
\description{
A step of the ibpfa_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{Z}, \code{order}, \code{note}.
}
\keyword{internal}
\examples{
left_ordered_form(Z = 5L)
}
