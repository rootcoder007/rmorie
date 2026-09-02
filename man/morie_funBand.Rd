% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_funBand}
\alias{morie_funBand}
\alias{morie_functional_band}
\title{Smoothing-spline fit with Wahba\'s Bayesian confidence intervals}
\usage{
morie_funBand(Y, alpha = 0.05, x = NULL, lam = NULL, quantile = "t", truth = NULL, 
    n_lambda = 40L, log_lambda_range = c(-8, 8)) 

morie_functional_band(Y, alpha = 0.05, x = NULL, lam = NULL, quantile = "t", truth = NULL, 
    n_lambda = 40L, log_lambda_range = c(-8, 8)) 
}
\description{
Smoothing-spline fit with Wahba\'s Bayesian confidence intervals.
}
\arguments{
\item{Y}{Argument \code{Y}; see Usage.}
\item{alpha}{Argument \code{alpha}; see Usage.}
\item{x}{Argument \code{x}; see Usage.}
\item{lam}{Argument \code{lam}; see Usage.}
\item{quantile}{Argument \code{quantile}; see Usage.}
\item{truth}{Argument \code{truth}; see Usage.}
\item{n_lambda}{Argument \code{n_lambda}; see Usage.}
\item{log_lambda_range}{Argument \code{log_lambda_range}; see Usage.}
}
\value{
A list with \code{estimate}, \code{fitted}, \code{lower}, \code{upper}, \code{half_width}, \code{residuals}, \code{diag_A}, \code{posterior_variance}, \code{sigma2}, \code{sigma}, \code{lambda}, \code{gcv}, \code{edf_signal}, \code{edf_error}, \code{rss}, \code{multiplier}, \code{quantile}, \code{coverage}, \code{alpha}, \code{n}, \code{x}, \code{method}, \code{note}.
}
\references{
Wahba, G. (1983) "Bayesian \'confidence intervals\' for the cross-validated
smoothing spline", Journal of the Royal Statistical Society Series B
(Methodological) 45(1), 133-150, doi:10.1111/j.2517-6161.1983.tb01239.x.
Craven, P. and Wahba, G. (1979) "Smoothing noisy data with spline functions",
Numerische Mathematik 31(4), 377-403, doi:10.1007/BF01404567.
Green, P. J. and Silverman, B. W. (1994) Nonparametric Regression and
Generalized Linear Models, Chapman and Hall, ISBN 978-0-412-30040-0, Sec 2.1.2.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_funBand(V)
}
