% Generated from the module source by ledger/gen_rd.R -- roxygen2 hangs
% on this package. Usage comes from args(), so it tracks the formals.
\name{morie_plsqs_pls_regression}
\alias{morie_plsqs_pls_regression}
\alias{morie_plsqs}
\title{PLS1 regression by NIPALS}
\usage{
morie_plsqs_pls_regression(X, Y, n_components = 2)
}
\description{
R arm of plsqs -- PLS1 regression by NIPALS. of chemometrics", Chemometrics and Intelligent Laboratory Systems 58(2),
}
\value{
A list with components \code{n}, \code{a}, \code{estimate}, \code{coefficients}, \code{intercept}, \code{fitted}, \code{residuals}, \code{scores}, \code{weights}, \code{loadings}, \code{y_loadings}, \code{explained_x}, \code{explained_y}, \code{n_components}, \code{r_squared}, \code{p}, \code{method}, \code{note}.
}
\references{
Wold, S., Sjostrom, M. & Eriksson, L. (2001) "PLS-regression: a basic tool
109-130, doi:10.1016/S0169-7439(01)00155-1.
}
\keyword{internal}
