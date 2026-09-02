% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Facrisk}
\alias{Facrisk}
\title{Spatial factor Poisson log-risk (Lawson eq. 11.1)}
\usage{
Facrisk(alpha0, W, phi) 
}
\description{
Spatial factor Poisson log-risk (Lawson eq. 11.1)
}
\arguments{
\item{alpha0}{Argument \code{alpha0}; see Usage.}
\item{W}{Argument \code{W}; see Usage.}
\item{phi}{Argument \code{phi}; see Usage.}
}
\value{
A list with \code{logrisk}, \code{risk}, \code{n}, \code{n_components}.
}
\keyword{internal}
\examples{
Facrisk(alpha0 = c(1, 2, 3, 4, 5, 6, 7, 8), W = c(1, 2, 3, 4, 5, 6, 7, 8), phi = 0.5)
}
