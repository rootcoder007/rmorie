% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Ksvmdual}
\alias{Ksvmdual}
\title{Support vector machine with a kernel (MVSML eqs. 9.46-9.47)}
\usage{
Ksvmdual(X, y, T, kernel = "linear", gamma = NULL, K = NULL, ...) 
}
\description{
Because the dual touches the data only through the inner products x_i . x_j, each can be replaced by a positive definite symmetric kernel K(x_i, x_j), which implicitly defines an inner product in an enlarged feature space.  That substitution is the whole difference between (9.44) and (9.46); the constraints (9.47) are unchanged.
}
\arguments{
\item{X}{Argument \code{X}; see Usage.}
\item{y}{Argument \code{y}; see Usage.}
\item{T}{Argument \code{T}; see Usage.}
\item{kernel}{Argument \code{kernel}; see Usage.}
\item{gamma}{Argument \code{gamma}; see Usage.}
\item{K}{Argument \code{K}; see Usage.}
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
rmorie:::Ksvmdual(X = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8),
  T = c(1, 2, 3, 4, 5, 6, 7, 8))
}
