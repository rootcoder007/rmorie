% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_is_psd}
\alias{morie_is_psd}
\title{morie_is_psd}
\usage{
morie_is_psd(K, tol = 1e-09) 
}
\description{
morie_is_psd
}
\arguments{
\item{K}{Argument \code{K}; see Usage.}
\item{tol}{Argument \code{tol}; see Usage.}
}
\value{
A list with \code{psd}, \code{eigenvalues}.
}
\keyword{internal}
\examples{
rmorie:::morie_is_psd(K = 5L)
}
