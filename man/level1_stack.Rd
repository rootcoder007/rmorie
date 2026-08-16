% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{level1_stack}
\alias{level1_stack}
\title{level1_stack}
\usage{
level1_stack(preds, y, cv = "kfold", k = 5, lam = NULL)
}
\arguments{
\item{preds}{A vector; its length is taken.}
\item{y}{A vector; its length is taken and its elements indexed.}
\item{cv}{Passed to \code{.regmlm_folds}. Defaults to \code{"kfold"}.}
\item{k}{Passed to \code{.regmlm_folds}. Defaults to \code{5}.}
\item{lam}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the regmlm_native implementation. Called by \code{morie_regmlm}. See the file header for the source the module follows.
}
\value{
A list with \code{weights}, \code{prediction}, \code{out_of_fold}, \code{cv}, \code{lam}, \code{n_predictors}.
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
level1_stack(preds = list(a = 1, b = 2), y = 5L)
}
