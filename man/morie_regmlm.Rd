% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_regmlm}
\alias{morie_regmlm}
\title{morie_regmlm}
\usage{
morie_regmlm(G, y, chromosomes = NULL, block_size = 1000, n_ridge = 5, cv = "kfold", k = 5)
}
\arguments{
\item{G}{A matrix; passed to \code{nrow}.}
\item{y}{A vector; its length is taken.}
\item{chromosomes}{Passed to \code{make_blocks}.}
\item{block_size}{Passed to \code{make_blocks}. Defaults to \code{1000}.}
\item{n_ridge}{Passed to \code{level0_predictors}. Defaults to \code{5}.}
\item{cv}{Passed to \code{level1_stack}. Defaults to \code{"kfold"}.}
\item{k}{Passed to \code{level1_stack}. Defaults to \code{5}.}
}
\description{
A step of the regmlm_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{blocks}, \code{n_blocks}, \code{level0}, \code{level1}, \code{loco}, \code{chromosomes}, \code{n_predictors}, \code{reduction}, \code{method}.
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
r <- morie_regmlm(G, y, block_size = 10, n_ridge = 3)
str(r, max.level = 1)
}
