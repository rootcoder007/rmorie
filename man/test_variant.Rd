% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{test_variant}
\alias{test_variant}
\title{test_variant}
\usage{
test_variant(g, y, offset = NULL, covariates = list()) 
}
\arguments{
\item{g}{A vector; its length is taken.}
\item{y}{A vector; its length is taken.}
\item{offset}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{covariates}{A vector; its length is taken. Defaults to \code{list()}.}
}
\description{
A step of the regmlm_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{beta}, \code{se}, \code{chisq}, \code{p_value}, \code{n}.
}
\references{
Mbatchou, J., et al. (2021) "Computationally efficient whole-genome
regression for quantitative and binary traits", Nature Genetics
53(7), 1097-1103, doi:10.1038/s41588-021-00870-7.

Hoerl, A. E. & Kennard, R. W. (1970) "Ridge Regression: Biased
Estimation for Nonorthogonal Problems", Technometrics 12(1), 55-67.

Yang, J., et al. (2014) "Advantages and pitfalls in the application
of mixed-model association methods", Nature Genetics 46(2),
100-106.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
test_variant(V, V)
}
