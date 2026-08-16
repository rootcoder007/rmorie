% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.hrz3_kmat}
\alias{.hrz3_kmat}
\title{Gaussian kernel matrix K((a_i - b_j)/h)}
\usage{
.hrz3_kmat(a, b, h)
}
\arguments{
\item{a}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{b}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{h}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the helpers_hrz3 implementation. Called by \code{.hrz3_fxw_grid}, \code{Hrznpiv}, \code{Hrzplrq} and 1 others in the module. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.hrz3_kmat(a = c(1, 2, 3, 4, 5, 6, 7, 8), b = 5L, h = 0.5)
}
