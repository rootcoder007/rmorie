% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{HannFr}
\alias{HannFr}
\title{Eq (3.104): the transfer function on the unit circle, raw form}
\usage{
HannFr(omega) 
}
\arguments{
\item{omega}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the rangayyan_filt implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{H}, \code{omega}, \code{magnitude}, \code{on_the_unit_circle}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
HannFr(V)
}
