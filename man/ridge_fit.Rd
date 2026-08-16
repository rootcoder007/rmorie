% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ridge_fit}
\alias{ridge_fit}
\title{ridge_fit}
\usage{
ridge_fit(X, y, lam)
}
\arguments{
\item{X}{A matrix; passed to \code{nrow}.}
\item{y}{A matrix; passed to \code{crossprod}.}
\item{lam}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the regmlm_native implementation. Called by \code{level0_predictors}, \code{level1_stack}. See the file header for the source the module follows.
}
\value{
A list with \code{beta}, \code{fitted}, \code{lam}.
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
