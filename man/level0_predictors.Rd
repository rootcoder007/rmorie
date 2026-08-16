% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{level0_predictors}
\alias{level0_predictors}
\title{level0_predictors}
\usage{
level0_predictors(G, y, blocks, n_ridge = 5)
}
\arguments{
\item{G}{See Usage.}
\item{y}{See Usage.}
\item{blocks}{See Usage.}
\item{n_ridge}{Defaults to \code{5}.}
}
\description{
Part of the regmlm_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{predictors}, \code{meta}, \code{n_predictors}, \code{reduction}.
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
