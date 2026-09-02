% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{alfSmax}
\alias{alfSmax}
\title{Softmax with max subtraction.  The stabilisation is part of the}
\usage{
alfSmax(v) 
}
\arguments{
\item{v}{Numeric; passed to \code{max}.}
}
\description{
contract: the Python arm subtracts the same maximum.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
alfSmax(V)
}
