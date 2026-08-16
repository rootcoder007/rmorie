% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{transform_to_real}
\alias{transform_to_real}
\title{ADVI\'s bijection to \code{R} with its log-Jacobian}
\usage{
transform_to_real(value, support = "positive", eps = 1e-10)
}
\arguments{
\item{value}{Numeric scalar.}
\item{support}{One of \code{"positive"}, \code{"unit"},   \code{"real"}.}
\item{eps}{Positive tolerance; default \code{1e-10}. Not a tuning   knob for the R arm.}
}
\description{
Fitting a Gaussian to a positive parameter directly puts mass on impossible values; transforming first is what makes one automatic recipe cover every model.
}
\value{
A list with \code{real, log_jacobian, inverse}.
}
\keyword{internal}
\examples{
transform_to_real(value = 5L)
}
