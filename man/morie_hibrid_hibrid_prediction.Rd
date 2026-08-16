% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_hibrid_hibrid_prediction}
\alias{morie_hibrid_hibrid_prediction}
\title{morie_hibrid_hibrid_prediction}
\usage{
morie_hibrid_hibrid_prediction(y, p1_geno, p2_geno, sigma2_sca = NULL, X = NULL, p1_new = NULL, p2_new = NULL, max_iter = 300L, tol = 1e-10)
}
\arguments{
\item{y}{See Usage.}
\item{p1_geno}{See Usage.}
\item{p2_geno}{See Usage.}
\item{sigma2_sca}{Defaults to \code{NULL}.}
\item{X}{Defaults to \code{NULL}.}
\item{p1_new}{Defaults to \code{NULL}.}
\item{p2_new}{Defaults to \code{NULL}.}
\item{max_iter}{Defaults to \code{300L}.}
\item{tol}{Defaults to \code{1e-10}.}
}
\description{
Part of the hibrid_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{fitted}, \code{gca_effect}, \code{sca_effect}, \code{coefficients}, \code{sigma2_gca}, \code{sigma2_sca}, \code{sigma2_e}, \code{sca_share}, \code{h2}, \code{gca_kernel}, \code{sca_kernel}, \code{reml_path}, \code{reml_loglik}, \code{iterations}, \code{converged}, \code{sca_fixed}, \code{prediction_new}, \code{residuals}, \code{n}, \code{m}, \code{p}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
morie_hibrid_hibrid_prediction(y = c(1, 2, 3, 4, 5, 6, 7, 8), p1_geno = c(1, 2, 3, 4, 5, 6, 7, 8), p2_geno = c(1, 2, 3, 4, 5, 6, 7, 8))
}
