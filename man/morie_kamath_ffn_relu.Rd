% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_kamath_ffn_relu}
\alias{morie_kamath_ffn_relu}
\title{morie_kamath_ffn_relu}
\usage{
morie_kamath_ffn_relu(z, W_1, W_2, b_1, b_2) 
}
\arguments{
\item{z}{Input rows.}
\item{W_1,W_2,b_1,b_2}{FFN parameters.}
}
\description{
morie_kamath_ffn_relu
}
\value{
A list with \code{output}, \code{hidden}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
morie_kamath_ffn_relu(rbind(c(1, -1)), rbind(c(1, 0), c(0, 1)), 
    rbind(1, 1), c(0, 0), 0.5)
}
