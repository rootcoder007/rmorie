% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_miprgr_fractional_variable}
\alias{morie_miprgr_fractional_variable}
\title{Most fractional integer variable}
\usage{
morie_miprgr_fractional_variable(x, integer_vars, tol = .GHC_MIP_EPS)
}
\arguments{
\item{x}{Solution vector.}
\item{integer_vars}{Indices that must be integral.}
\item{tol}{Tolerance.}
}
\description{
Most fractional integer variable
}
\value{
A list with index, fractionality, integral.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_miprgr_fractional_variable(V, V)
}
