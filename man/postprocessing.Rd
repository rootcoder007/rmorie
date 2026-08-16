% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{postprocessing}
\alias{postprocessing}
\title{Lemma 1.8: any function of the output keeps the same rho}
\usage{
postprocessing(rho)
}
\arguments{
\item{rho}{zCDP parameter, must be positive.}
}
\description{
Lemma 1.8: any function of the output keeps the same rho
}
\value{
A list with \code{rho} and a \code{note} explaining that   the parameter is invariant under post-processing -- unlike   Dwork and Rothblum\'s mCDP, which is not closed under   post-processing.
}
\keyword{internal}
