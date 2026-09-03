% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_hibrid_hibrid_prediction}
\alias{morie_hibrid_hibrid_prediction}
\title{morie_hibrid_hibrid_prediction}
\usage{
morie_hibrid_hibrid_prediction(y, p1_geno, p2_geno, sigma2_sca = NULL, X = NULL, p1_new = NULL, 
    p2_new = NULL, max_iter = 300L, tol = 1e-10) 
}
\arguments{
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{p1_geno}{Passed to \code{.hibrid_rows}.}
\item{p2_geno}{Passed to \code{.hibrid_rows}.}
\item{sigma2_sca}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{X}{Optional; may be \code{NULL}. Passed to \code{.hibrid_rows}.}
\item{p1_new}{Optional; may be \code{NULL}. Passed to \code{.hibrid_rows}.}
\item{p2_new}{Optional; may be \code{NULL}. Passed to \code{.hibrid_rows}.}
\item{max_iter}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{300L}.}
\item{tol}{Accepted by the signature and not used anywhere in the body. Defaults to \code{1e-10}.}
}
\description{
A step of the hibrid_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{fitted}, \code{gca_effect}, \code{sca_effect}, \code{coefficients}, \code{sigma2_gca}, \code{sigma2_sca}, \code{sigma2_e}, \code{sca_share}, \code{h2}, \code{gca_kernel}, \code{sca_kernel}, \code{reml_path}, \code{reml_loglik}, \code{iterations}, \code{converged}, \code{sca_fixed}, \code{prediction_new}, \code{residuals}, \code{n}, \code{m}, \code{p}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
morie_hibrid_hibrid_prediction(y = c(1, 2, 3, 4, 5, 6, 7, 8),
  p1_geno = c(1, 2, 3, 4, 5, 6, 7, 8), p2_geno = c(1, 2, 3, 4, 5, 6, 7, 8))
}
