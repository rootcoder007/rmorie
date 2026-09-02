% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{violates_kkt}
\alias{violates_kkt}
\title{violates_kkt}
\usage{
violates_kkt(i, alpha, y, E, C, tol = 0.001) 
}
\arguments{
\item{i}{See Usage.}
\item{alpha}{A vector; indexed elementwise.}
\item{y}{A vector; indexed elementwise.}
\item{E}{A vector; indexed elementwise.}
\item{C}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{tol}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.001}.}
}
\description{
A step of the smoopt_native implementation. Called by \code{smo_platt}. See the file header for the source the module follows.
}
\value{
A logical value.
}
\keyword{internal}
\examples{
violates_kkt(i = 5L, alpha = 0.5, y = c(1, 2, 3, 4, 5, 6, 7, 8), E = c(1, 2, 3, 4, 5, 6, 7, 8), C = c(1, 2, 3, 4, 5, 6, 7, 8))
}
