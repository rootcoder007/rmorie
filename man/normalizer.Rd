% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{normalizer}
\alias{normalizer}
\title{Z: the value of the worst arrangement. The protected group placed}
\usage{
normalizer(protected, measure = "rND", step = 10)
}
\arguments{
\item{protected}{A vector; its length is taken.}
\item{measure}{Passed to \code{.raw}. Defaults to \code{"rND"}.}
\item{step}{Passed to \code{.raw}. Defaults to \code{10}.}
}
\description{
entirely last maximises the deviation at every cut-off.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
set.seed(1)
normalizer(rep(c(1L, 0L), 25), measure = "rND", step = 10)
}
