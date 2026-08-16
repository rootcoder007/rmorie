% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{LmsOut}
\alias{LmsOut}
\title{Eq. (3.195): the adaptive filter runs on the REFERENCE, not on the}
\usage{
LmsOut(w, r)
}
\arguments{
\item{w}{See Usage.}
\item{r}{See Usage.}
}
\description{
primary input.  Filtering the primary would cancel the signal too.
}
\value{
A list with \code{y}, \code{n}, \code{order}, \code{settled_from}, \code{filters_the_reference}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
LmsOut(V, V)
}
