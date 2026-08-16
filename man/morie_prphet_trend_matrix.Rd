% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_prphet_trend_matrix}
\alias{morie_prphet_trend_matrix}
\title{morie_prphet_trend_matrix}
\usage{
morie_prphet_trend_matrix(t, cps)
}
\arguments{
\item{t}{A vector; its length is taken.}
\item{cps}{A vector; its length is taken and its elements indexed.}
}
\description{
A step of the prphet_native implementation. Called by \code{morie_prphet_design}. See the file header for the source the module follows.
}
\value{
The value of \code{rows}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_prphet_trend_matrix(V, V)
}
