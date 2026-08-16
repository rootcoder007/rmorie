% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.dkernel_dr2}
\alias{.dkernel_dr2}
\title{.dkernel_dr2}
\usage{
.dkernel_dr2(name, amplitude, r2)
}
\arguments{
\item{name}{Compared against \code{"se"}.}
\item{amplitude}{Numeric; combined arithmetically in the body.}
\item{r2}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the bayopt_native implementation. Called by \code{gp_posterior_gradient}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.dkernel_dr2(name = 5L, amplitude = c(1, 2, 3, 4, 5, 6, 7, 8), r2 = c(1, 2, 3, 4, 5, 6, 7, 8))
}
