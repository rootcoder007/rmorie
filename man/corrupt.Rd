% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{corrupt}
\alias{corrupt}
\title{corrupt}
\usage{
corrupt(y, q, rng) 
}
\arguments{
\item{y}{A vector; its length is taken.}
\item{q}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{rng}{Passed to \code{.ghc_unif}.}
}
\description{
A step of the cdaeRC_native implementation. Called by \code{fit_cdae}. See the file header for the source the module follows.
}
\value{
The value of \code{ifelse}.
}
\keyword{internal}
\examples{
rng <- .ghc_rng(1)
corrupt(c(1, 0, 1, 1, 0), q = 0.4, rng)
}
