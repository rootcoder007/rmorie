% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_evt_return_level_ci}
\alias{morie_evt_return_level_ci}
\title{Delta-method CI for a GEV return level (Coles 2001 eq. 3.10-3.11)}
\usage{
morie_evt_return_level_ci(x, T, alpha = 0.05) 
}
\arguments{
\item{x}{block maxima to fit}
\item{T}{return period}
\item{alpha}{1 - confidence level}
}
\description{
Delta-method CI for a GEV return level (Coles 2001 eq. 3.10-3.11)
}
\value{
A list with \code{z_T}, \code{ci_lo}, \code{ci_hi}, \code{se}.
}
\keyword{internal}
\examples{
morie_evt_return_level_ci(x = c(1, 2, 3, 4, 5, 6, 7, 8), T = 5L)
}
