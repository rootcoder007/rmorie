% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Qchisq}
\alias{Qchisq}
\title{Qchisq}
\usage{
Qchisq(p, df) 
}
\arguments{
\item{p}{Passed to \code{Qgamma}.}
\item{df}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the dist_native implementation. Called by \code{.rsconsistency}. See the file header for the source the module follows.
}
\value{
The value of \code{Qgamma}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Qchisq(V, V)
}
