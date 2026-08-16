% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Qnorm}
\alias{Qnorm}
\title{Qnorm}
\usage{
Qnorm(p, mean = 0, sd = 1, lower_tail = TRUE)
}
\arguments{
\item{p}{Numeric; combined arithmetically in the body.}
\item{mean}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
\item{sd}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{lower_tail}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the dist_native implementation. Called by \code{Rnorm}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
Qnorm(p = 0.5)
}
