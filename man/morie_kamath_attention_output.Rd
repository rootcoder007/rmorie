% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_kamath_attention_output}
\alias{morie_kamath_attention_output}
\title{morie_kamath_attention_output}
\usage{
morie_kamath_attention_output(b, v) 
}
\arguments{
\item{b}{Weights.}
\item{v}{Value matrix, one row per weight.}
}
\description{
morie_kamath_attention_output
}
\value{
A list with \code{output}, \code{is_convex_combination}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_kamath_attention_output(V, V)
}
