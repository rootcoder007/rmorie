% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{network_influence_variance}
\alias{network_influence_variance}
\title{Variance with covariance along edges only}
\usage{
network_influence_variance(ic, friends) 
}
\description{
Variance with covariance along edges only.
}
\arguments{
\item{ic}{Argument \code{ic}; see Usage.}
\item{friends}{Argument \code{friends}; see Usage.}
}
\value{
A list with \code{se}, \code{se_independent}, \code{edges_counted}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
network_influence_variance(V, V)
}
