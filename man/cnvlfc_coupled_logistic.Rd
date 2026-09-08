% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{cnvlfc_coupled_logistic}
\alias{cnvlfc_coupled_logistic}
\title{cnvlfc_coupled_logistic}
\usage{
cnvlfc_coupled_logistic(n, rx = 3.8, ry = 3.5, bxy = 0, byx = 0.1, x0 = 0.4, y0 = 0.2, 
    burn = 300L) 
}
\arguments{
\item{n}{Passed to \code{.cnvlfc_coupled_logistic}.}
\item{rx}{Passed to \code{.cnvlfc_coupled_logistic}. Defaults to \code{3.8}.}
\item{ry}{Passed to \code{.cnvlfc_coupled_logistic}. Defaults to \code{3.5}.}
\item{bxy}{Passed to \code{.cnvlfc_coupled_logistic}. Defaults to \code{0}.}
\item{byx}{Passed to \code{.cnvlfc_coupled_logistic}. Defaults to \code{0.1}.}
\item{x0}{Passed to \code{.cnvlfc_coupled_logistic}. Defaults to \code{0.4}.}
\item{y0}{Passed to \code{.cnvlfc_coupled_logistic}. Defaults to \code{0.2}.}
\item{burn}{Passed to \code{.cnvlfc_coupled_logistic}. Defaults to \code{300L}.}
}
\description{
A step of the cnvlfc_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{.cnvlfc_coupled_logistic}.
}
\keyword{internal}
\examples{
cnvlfc_coupled_logistic(n = 5L)
}
