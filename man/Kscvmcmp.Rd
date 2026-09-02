% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Kscvmcmp}
\alias{Kscvmcmp}
\title{KS vs Cramer-von Mises on one sample -- Sec. 4.9, p. 146}
\usage{
Kscvmcmp(x, cdf) 
}
\description{
KS vs Cramer-von Mises on one sample -- Sec. 4.9, p. 146
}
\arguments{
\item{x}{Argument \code{x}; see Usage.}
\item{cdf}{Argument \code{cdf}; see Usage.}
}
\value{
A list with \code{d}, \code{w2}, \code{argmax}, \code{share}, \code{n}.
}
\keyword{internal}
\examples{
set.seed(2)
Kscvmcmp(rnorm(30), cdf = pnorm)
}
