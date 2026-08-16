% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Pnorm}
\alias{Pnorm}
\title{Pnorm}
\usage{
Pnorm(q, mean = 0, sd = 1, lower_tail = TRUE)
}
\arguments{
\item{q}{See Usage.}
\item{mean}{Defaults to \code{0}.}
\item{sd}{Defaults to \code{1}.}
\item{lower_tail}{Defaults to \code{TRUE}.}
}
\description{
Part of the dist_native implementation; see the file header for the source it follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Pnorm(V)
}
