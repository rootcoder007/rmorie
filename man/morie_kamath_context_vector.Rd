% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_kamath_context_vector}
\alias{morie_kamath_context_vector}
\title{morie_kamath_context_vector}
\usage{
morie_kamath_context_vector(h_1_h_T, mapping = "mean")
}
\arguments{
\item{h_1_h_T}{State matrix, one row per step.}
\item{mapping}{   "mean", "last", "max" or a function.}
}
\description{
morie_kamath_context_vector
}
\value{
A list with \code{context}, \code{mapping}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
