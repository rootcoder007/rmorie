% Generated from the module source by ledger/gen_rd.R -- roxygen2 hangs
% on this package. Usage comes from args(), so it tracks the formals.
\name{morie_funCA_functional_cca}
\alias{morie_funCA_functional_cca}
\alias{morie_funCA}
\title{Functional canonical analysis for two square-integrable}
\usage{
morie_funCA_functional_cca(X, Y, p = NULL, q = NULL)
}
\description{
R arm of funCA -- functional canonical analysis for two square-integrable processes, restricted to the leading functional principal components. for square integrable stochastic processes", Journal of Multivariate the supremum is 1 for almost any pair of processes -- so the weight functions are confined to the span of the leading FPCs of each process and the truncation (p, q) is reported with the correlations.
}
\value{
A list with components \code{n}, \code{symmetric}, \code{decreasing}, \code{estimate}, \code{correlations}, \code{weights_x}, \code{weights_y}, \code{variates_x}, \code{variates_y}, \code{p}, \code{q}, \code{explained_x}, \code{explained_y}, \code{eigenvalues_x}, \code{eigenvalues_y}, \code{method}, \code{note}.
}
\references{
He, G., Muller, H.-G. & Wang, J.-L. (2003) "Functional canonical analysis
Analysis 85(1), 54-77, doi:10.1016/S0047-259X(02)00056-8.
}
\keyword{internal}
