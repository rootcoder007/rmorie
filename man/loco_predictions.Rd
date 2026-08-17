% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{loco_predictions}
\alias{loco_predictions}
\title{loco_predictions}
\usage{
loco_predictions(preds, meta, weights, chromosomes = NULL)
}
\arguments{
\item{preds}{A vector; indexed elementwise.}
\item{meta}{Iterated over elementwise, with \code{sapply}.}
\item{weights}{A vector; indexed elementwise.}
\item{chromosomes}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the regmlm_native implementation. Called by \code{morie_regmlm}. See the file header for the source the module follows.
}
\value{
A list with \code{loco}, \code{chromosomes}, \code{note}.
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
blocks <- make_blocks(20, chromosomes = rep(1:2, each = 10),
                      block_size = 10)
l0 <- level0_predictors(G, y, blocks, n_ridge = 3)
l1 <- level1_stack(l0$predictors, y)
r <- loco_predictions(l0$predictors, l0$meta, l1$weights)
str(r, max.level = 1)
}
