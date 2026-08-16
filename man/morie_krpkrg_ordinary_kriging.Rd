% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_krpkrg_ordinary_kriging}
\alias{morie_krpkrg_ordinary_kriging}
\title{morie_krpkrg_ordinary_kriging}
\usage{
morie_krpkrg_ordinary_kriging(coords, values, targets, model = "spherical", nugget = 0, sill = 1, rng = 1)
}
\arguments{
\item{coords}{See Usage.}
\item{values}{See Usage.}
\item{targets}{See Usage.}
\item{model}{Defaults to \code{"spherical"}.}
\item{nugget}{Defaults to \code{0}.}
\item{sill}{Defaults to \code{1}.}
\item{rng}{Defaults to \code{1}.}
}
\description{
Part of the krpkrg_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{prediction}, \code{variance}, \code{std_error}, \code{weights}, \code{n}, \code{n_targets}, \code{model}, \code{nugget}, \code{sill}, \code{range}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
morie_krpkrg_ordinary_kriging(coords = c(1, 2, 3, 4, 5, 6, 7, 8), values = c(1, 2, 3, 4, 5, 6, 7, 8), targets = c(1, 2, 3, 4, 5, 6, 7, 8))
}
