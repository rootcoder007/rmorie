% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{local_nuisance}
\alias{local_nuisance}
\title{Weighted linear fit of `target` on W near a point. weights are the}
\usage{
local_nuisance(target, W, weights, exclude = NULL, ridge = 1e-08)
}
\arguments{
\item{target}{See Usage.}
\item{W}{See Usage.}
\item{weights}{See Usage.}
\item{exclude}{Defaults to \code{NULL}.}
\item{ridge}{Defaults to \code{1e-08}.}
}
\description{
forest kernel weights alpha_i(x), so the fit is local by construction. exclude drops one index for the leave-one-out residual.
}
\value{
A list with \code{fit}, \code{coef}.
}
\keyword{internal}
\examples{
local_nuisance(target = c(1, 2, 3, 4, 5, 6, 7, 8), W = c(1, 2, 3, 4, 5, 6, 7, 8), weights = c(1, 2, 3, 4, 5, 6, 7, 8))
}
