% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.singular_value_adjust}
\alias{.singular_value_adjust}
\title{.singular_value_adjust}
\usage{
.singular_value_adjust(a, cap = 0.97)
}
\arguments{
\item{a}{Passed to \code{.svd_r}.}
\item{cap}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.97}.}
}
\description{
A step of the andmnh_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{\%*\%}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.singular_value_adjust(V)
}
