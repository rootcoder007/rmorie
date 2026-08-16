% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_likemc_poisll}
\alias{.morie_likemc_poisll}
\title{.morie_likemc_poisll}
\usage{
.morie_likemc_poisll(observed, expected)
}
\arguments{
\item{observed}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{expected}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the likemc_native implementation. Called by \code{morie_likemc}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_likemc_poisll(V, V)
}
