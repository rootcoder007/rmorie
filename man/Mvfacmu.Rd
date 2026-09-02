% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Mvfacmu}
\alias{Mvfacmu}
\title{Shared-factor multivariate disease mean (Lawson eq. 14.1)}
\usage{
Mvfacmu(e, lam, f) 
}
\description{
Shared-factor multivariate disease mean (Lawson eq. 14.1)
}
\arguments{
\item{e}{Argument \code{e}; see Usage.}
\item{lam}{Argument \code{lam}; see Usage.}
\item{f}{Argument \code{f}; see Usage.}
}
\value{
A list with \code{rho}, \code{mu}, \code{n}, \code{n_disease}.
}
\keyword{internal}
\examples{
rmorie:::Mvfacmu(e = c(1, 2, 3, 4, 5, 6, 7, 8), lam = 5L, f = c(1, 2, 3, 4, 5, 6, 7, 8))
}
