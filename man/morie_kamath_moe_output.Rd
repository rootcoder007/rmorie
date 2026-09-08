% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_kamath_moe_output}
\alias{morie_kamath_moe_output}
\title{morie_kamath_moe_output}
\usage{
morie_kamath_moe_output(x, G, E_i) 
}
\arguments{
\item{x}{Input.}
\item{G}{Gate weights or function.}
\item{E_i}{Expert functions or precomputed outputs.}
}
\description{
morie_kamath_moe_output
}
\value{
A list with \code{output}, \code{gate_weights}, \code{experts_evaluated}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
morie_kamath_moe_output(x = c(1, 2, 3, 4, 5, 6, 7, 8), G = c(1, 2, 3, 4, 5, 6, 7, 8),
  E_i = c(1, 2, 3, 4, 5, 6, 7, 8))
}
