% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Pchisq}
\alias{Pchisq}
\title{Pchisq}
\usage{
Pchisq(q, df, lower_tail = TRUE)
}
\arguments{
\item{q}{See Usage.}
\item{df}{Numeric; combined arithmetically in the body.}
\item{lower_tail}{Defaults to \code{TRUE}.}
}
\description{
A step of the dist_native implementation. Called by \code{.rsconsistency}. See the file header for the source the module follows.
}
\value{
The value of \code{Pgamma}.
}
\keyword{internal}
\examples{
Pchisq(3.84, 1)
}
