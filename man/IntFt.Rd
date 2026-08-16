% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{IntFt}
\alias{IntFt}
\title{Eq (3.115): Y(w) = X(w)/(jw) + pi X(0) delta(w).  The delta carries}
\usage{
IntFt(X, omega, X0 = NULL)
}
\arguments{
\item{X}{See Usage.}
\item{omega}{See Usage.}
\item{X0}{Defaults to \code{NULL}.}
}
\description{
the DC content, which 1/(jw) cannot represent because it blows up there.  It is returned as its WEIGHT, pi X(0); a delta has no value at a point.
}
\value{
A list with \code{Y}, \code{omega}, \code{delta_weight}, \code{at_dc}, \code{dc_term_carried_by_the_delta}, \code{undefined_at_zero_without_the_delta}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
IntFt(V, V)
}
