% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{S_function}
\alias{S_function}
\title{S_function}
\usage{
S_function(std_moments, form = "sum", n_equality = 0L)
}
\arguments{
\item{std_moments}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{form}{One of \code{"max"}, \code{"sum"}. Defaults to \code{"sum"}.}
\item{n_equality}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{0L}.}
}
\description{
A step of the bnskmt_native implementation. Called by \code{cvm_statistic}, \code{ks_critical_value}, \code{ks_statistic}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
S_function(V)
}
