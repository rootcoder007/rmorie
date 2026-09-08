% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_S_function}
\alias{morie_S_function}
\title{morie_S_function}
\usage{
morie_S_function(std_moments, form = "sum", n_equality = 0L) 
}
\arguments{
\item{std_moments}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{form}{One of \code{"max"}, \code{"sum"}. Defaults to \code{"sum"}.}
\item{n_equality}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{0L}.}
}
\description{
A step of the bndsmw_native implementation. Called by \code{morie_cvm_statistic}, \code{morie_gms_critical_value}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_S_function(V)
}
