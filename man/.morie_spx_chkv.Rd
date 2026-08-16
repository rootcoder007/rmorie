% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_spx_chkv}
\alias{.morie_spx_chkv}
\title{.morie_spx_chkv}
\usage{
.morie_spx_chkv(x, name = "x")
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{name}{Passed to \code{sprintf}. Defaults to \code{"x"}.}
}
\description{
A step of the sp_fill implementation. Called by \code{CrossSpec}, \code{LisaClust}, \code{LisaI} and 16 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{v}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_spx_chkv(V)
}
