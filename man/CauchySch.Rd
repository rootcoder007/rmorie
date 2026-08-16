% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{CauchySch}
\alias{CauchySch}
\title{Eq (4.44): |a.b| <= |a||b|.  The ratio of the two sides is the cosine}
\usage{
CauchySch(a, b)
}
\arguments{
\item{a}{See Usage.}
\item{b}{See Usage.}
}
\description{
between the vectors, which is exactly the correlation coefficient of eq (4.25) on mean-removed signals.
}
\value{
A list with \code{lhs}, \code{rhs}, \code{holds}, \code{cosine}, \code{equality}, \code{norm_a}, \code{norm_b}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
CauchySch(V, V)
}
