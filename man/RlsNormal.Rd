% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{RlsNormal}
\alias{RlsNormal}
\title{Eq. (3.207): the same form as Wiener-Hopf, but with time-averaged and}
\usage{
RlsNormal(phi, theta) 
}
\arguments{
\item{phi}{Passed to \code{WienerHopf}.}
\item{theta}{Passed to \code{WienerHopf}.}
}
\description{
exponentially weighted correlations.  Solving it outright each sample is what the matrix inversion lemma exists to avoid.
}
\value{
The value of \code{r}, as built in the body.
}
\keyword{internal}
\examples{
RlsNormal(phi = 0.5, theta = 0.5)
}
