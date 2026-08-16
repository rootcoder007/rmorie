% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_crfflt_frequency_response}
\alias{morie_crfflt_frequency_response}
\title{Magnitude of the frequency response of a symmetric weight vector}
\usage{
morie_crfflt_frequency_response(weights, omega)
}
\arguments{
\item{weights}{Numeric vector of weights.}
\item{omega}{Numeric, frequency.}
}
\description{
Computes \code{|sum_j w_j exp(-i omega j)|} with the weight vector centred on its middle element.
}
\value{
Numeric magnitude.
}
\keyword{internal}
