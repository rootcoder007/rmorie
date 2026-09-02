% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{alfQ2rot}
\alias{alfQ2rot}
\title{Non-unit quaternion (1, b, c, d) to a rotation matrix -- Algorithm 23}
\usage{
alfQ2rot(b, c, d) 
}
\arguments{
\item{b}{Numeric; combined arithmetically in the body.}
\item{c}{Numeric; combined arithmetically in the body.}
\item{d}{Numeric; combined arithmetically in the body.}
}
\description{
lines 2-3.  Fixing the leading component to 1 before normalisation both guarantees a unit quaternion and makes zero input the identity rotation.
}
\value{
A matrix, from \code{matrix}.
}
\keyword{internal}
\examples{
alfQ2rot(b = 5L, c = c(1, 2, 3, 4, 5, 6, 7, 8), d = 5L)
}
