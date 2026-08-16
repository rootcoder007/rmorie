% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Qf}
\alias{Qf}
\title{Qf}
\usage{
Qf(p, df1, df2)
}
\arguments{
\item{p}{See Usage.}
\item{df1}{See Usage.}
\item{df2}{See Usage.}
}
\description{
A step of the dist_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
\examples{
Qf(p = 1, df1 = c(1, 2, 3, 4, 5, 6, 7, 8), df2 = c(1, 2, 3, 4, 5, 6, 7, 8))
}
