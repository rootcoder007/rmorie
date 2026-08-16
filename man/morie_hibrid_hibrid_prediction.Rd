% Generated from the module source by ledger/gen_rd.R -- roxygen2 hangs
% on this package. Usage comes from args(), so it tracks the formals.
\name{morie_hibrid_hibrid_prediction}
\alias{morie_hibrid_hibrid_prediction}
\alias{morie_hibrid}
\title{Genomic hybrid prediction: additive GCA kernel from the}
\usage{
morie_hibrid_hibrid_prediction(y, p1_geno, p2_geno, sigma2_sca = NULL, X = NULL, p1_new = NULL, p2_new = NULL, max_iter = 300L, tol = 1e-10)
}
\description{
R arm of hibrid -- genomic hybrid prediction: additive GCA kernel from the sum of the parental relationship matrices, SCA kernel from their Hadamard product, variance ratios by a profiled cyclic fixed-grid search. Genetics 197(4), 1343-1355.
}
\value{
A list with components \code{n}, \code{la}, \code{ls}, \code{estimate}, \code{fitted}, \code{gca_effect}, \code{sca_effect}, \code{coefficients}, \code{sigma2_gca}, \code{sigma2_sca}, \code{sigma2_e}, \code{sca_share}, \code{h2}, \code{gca_kernel}, \code{sca_kernel}, \code{reml_path}, \code{reml_loglik}, \code{iterations}, \code{converged}, \code{sca_fixed}, \code{prediction_new}, \code{residuals}, \code{m}, \code{p}, \code{method}, \code{note}.
}
\references{
Sprague, G. F. & Tatum, L. A. (1942) J. Am. Soc. Agron. 34(10), 923-932;
Technow, F. et al. (2012) TAG 125(6), 1181-1194; Technow, F. et al. (2014)
}
\keyword{internal}
