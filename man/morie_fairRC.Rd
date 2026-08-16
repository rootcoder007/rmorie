% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_fairRC}
\alias{morie_fairRC}
\title{Morie entry point}
\usage{
morie_fairRC(protected, measure = "rND", step = 10, normalize = TRUE)
}
\arguments{
\item{protected}{Passed to \code{rND}.}
\item{measure}{One of \code{"rKL"}, \code{"rND"}. Defaults to \code{"rND"}.}
\item{step}{Passed to \code{rND}. Defaults to \code{10}.}
\item{normalize}{Passed to \code{rND}. Defaults to \code{TRUE}.}
}
\description{
A step of the fairRC_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
prot <- c(rep(0L, 10), rep(1L, 10), rep(0L, 10), rep(1L, 10))
morie_fairRC(prot, measure = "rND", step = 10)
}
