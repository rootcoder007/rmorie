% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Qexp}
\alias{Qexp}
\title{Qexp}
\usage{
Qexp(p, rate = 1)
}
\arguments{
\item{p}{Numeric; combined arithmetically in the body.}
\item{rate}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
}
\description{
A step of the dist_native implementation. Called by \code{Rexp}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
Qexp(p = 0.5)
}
