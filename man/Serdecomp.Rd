% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Serdecomp}
\alias{Serdecomp}
\title{Series decomposition and autocorrelation (Wu et al. 2021, Autoformer)}
\usage{
Serdecomp(x, kernel) 
}
\description{
Series decomposition and autocorrelation (Wu et al. 2021, Autoformer)
}
\arguments{
\item{x}{Argument \code{x}; see Usage.}
\item{kernel}{Argument \code{kernel}; see Usage.}
}
\value{
A list with \code{trend}, \code{seasonal}, \code{acf}, \code{kernel}, \code{n}.
}
\keyword{internal}
\examples{
set.seed(5)
x <- sin(2 * pi * (1:60) / 12) + 0.1 * (1:60) / 10 + rnorm(60, 0, 0.1)
str(Serdecomp(x, kernel = 5), max.level = 1)
}
