% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{rKL}
\alias{rKL}
\title{Normalised discounted KL divergence}
\usage{
rKL(protected, step = 10, normalize = TRUE)
}
\arguments{
\item{protected}{Passed to \code{.measure}.}
\item{step}{Passed to \code{.measure}. Defaults to \code{10}.}
\item{normalize}{Passed to \code{.measure}. Defaults to \code{TRUE}.}
}
\description{
A step of the fairRC_native implementation. Called by \code{morie_fairRC}. See the file header for the source the module follows.
}
\value{
The value of \code{.measure}.
}
\keyword{internal}
\examples{
prot <- c(rep(0L, 10), rep(1L, 10), rep(0L, 10), rep(1L, 10))
rKL(prot, step = 10)
}
