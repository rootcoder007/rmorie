% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.hrz_gauss_kernel_deriv}
\alias{.hrz_gauss_kernel_deriv}
\title{K\\'(u) for the Gaussian kernel; the sign lives in the derivative}
\usage{
.hrz_gauss_kernel_deriv(u)
}
\arguments{
\item{u}{Numeric; combined arithmetically in the body.}
}
\description{
itself, and adding a second leading minus flips the average derivative\\'s sign (measured -0.548 against a theoretical +0.564).
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.hrz_gauss_kernel_deriv(V)
}
