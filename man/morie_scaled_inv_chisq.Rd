% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_scaled_inv_chisq}
\alias{morie_scaled_inv_chisq}
\title{morie_scaled_inv_chisq}
\usage{
morie_scaled_inv_chisq(nu, S, n = 1L) 
}
\description{
morie_scaled_inv_chisq
}
\arguments{
\item{nu}{Argument \code{nu}; see Usage.}
\item{S}{Argument \code{S}; see Usage.}
\item{n}{Argument \code{n}; see Usage.}
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
rmorie:::morie_scaled_inv_chisq(V, V)
}
