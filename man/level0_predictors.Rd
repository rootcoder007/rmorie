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
\item{G}{A matrix; indexed by row and column.}
\item{y}{A vector; its length is taken.}
\item{blocks}{See Usage.}
\item{n_ridge}{Passed to \code{.regmlm_lambda_grid}. Defaults to \code{5}.}
}
\description{
A step of the regmlm_native implementation. Called by \code{morie_regmlm}. See the file header for the source the module follows.
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
\examples{
set.seed(2)
n <- 30
G <- matrix(rbinom(n * 20, 2, 0.3), n, 20)
y <- G[, 1] - G[, 10] + rnorm(n, 0, 0.5)
blocks <- make_blocks(20, block_size = 10)
r <- level0_predictors(G, y, blocks, n_ridge = 3)
str(r, max.level = 1)
}
