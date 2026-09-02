% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Sampquant}
\alias{Sampquant}
\title{Sample quantile as an order statistic -- Sec. 2.6, p. 42}
\usage{
Sampquant(x, p) 
}
\description{
Sample quantile as an order statistic -- Sec. 2.6, p. 42
}
\arguments{
\item{x}{Argument \code{x}; see Usage.}
\item{p}{Argument \code{p}; see Usage.}
}
\value{
A list with \code{estimate}, \code{r}, \code{n}, \code{p}, \code{u_mean}, \code{u_var}.
}
\keyword{internal}
\examples{
rmorie:::Sampquant(x = c(1, 2, 3, 4, 5, 6, 7, 8), p = 0.5)
}
