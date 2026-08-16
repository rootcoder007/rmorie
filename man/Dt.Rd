% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Dt}
\alias{Dt}
\title{Dt}
\usage{
Dt(x, df, log = FALSE)
}
\arguments{
\item{x}{Numeric; combined arithmetically in the body.}
\item{df}{Numeric; combined arithmetically in the body.}
\item{log}{A flag; the body branches on it. Defaults to \code{FALSE}.}
}
\description{
A step of the dist_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
Dt(0, 5)
}
