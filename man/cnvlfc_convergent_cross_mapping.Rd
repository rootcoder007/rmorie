% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{cnvlfc_convergent_cross_mapping}
\alias{cnvlfc_convergent_cross_mapping}
\title{cnvlfc_convergent_cross_mapping}
\usage{
cnvlfc_convergent_cross_mapping(x, y, E = 2L, tau = 1L, ...)
}
\arguments{
\item{x}{Carried through into a list the body builds.}
\item{y}{Carried through into a list the body builds.}
\item{E}{Carried through into a list the body builds. Defaults to \code{2L}.}
\item{tau}{Carried through into a list the body builds. Defaults to \code{1L}.}
\item{...}{Passed through.}
}
\description{
A step of the cnvlfc_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{do.call}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
cnvlfc_convergent_cross_mapping(V, V)
}
