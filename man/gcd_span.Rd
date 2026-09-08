% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{gcd_span}
\alias{gcd_span}
\title{gcd_span}
\usage{
gcd_span(scores) 
}
\arguments{
\item{scores}{See Usage.}
}
\description{
A step of the blstn_native implementation. Called by \code{morie_karlin_altschul}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
gcd_span(V)
}
