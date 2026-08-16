% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ghc_beta}
\alias{.ghc_beta}
\title{Internal: ratio-of-gammas beta, same RNG consumption as Python}
\usage{
.ghc_beta(e, a, b)
}
\arguments{
\item{e}{Passed to \code{.ghc_gamma}.}
\item{a}{Passed to \code{.ghc_gamma}.}
\item{b}{Passed to \code{.ghc_gamma}.}
}
\description{
A step of the pmpfit_native implementation. Called by \code{morie_pmpfit}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
