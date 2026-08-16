% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_jo_vec}
\alias{.morie_jo_vec}
\title{.morie_jo_vec}
\usage{
.morie_jo_vec(x, name = "x")
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{name}{Defaults to \code{"x"}.}
}
\description{
A step of the ts_joseph implementation. Called by \code{.morie_jo_pair}, \code{morie_adfur}, \code{morie_autocorf} and 23 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{v}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_jo_vec(V)
}
