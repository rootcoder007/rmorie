% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_mps_acceleration}
\alias{morie_geron_mps_acceleration}
\title{Apple MPS placement plan: dtype demotion cost, no Metal call made (Geron Ch 10, hmpmps)}
\usage{
morie_geron_mps_acceleration(tensor, dtype = NULL)
}
\arguments{
\item{tensor}{Data.}
\item{dtype}{Optional forced target dtype name.}
}
\description{
Apple MPS placement plan: dtype demotion cost, no Metal call made (Geron Ch 10, hmpmps)
}
\value{
A list with \code{tensor}, \code{source_dtype}, \code{dtype_on_device}, \code{downcast}, \code{max_abs_error}, \code{relative_error}, \code{overflow}, \code{unified_memory}, \code{nbytes}, \code{executes_on_metal}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
