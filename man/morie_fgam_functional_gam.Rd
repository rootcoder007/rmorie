% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_fgam_functional_gam}
\alias{morie_fgam_functional_gam}
\title{morie_fgam_functional_gam}
\usage{
morie_fgam_functional_gam(X, Y, basis = NULL, n_x = 6, n_t = 6, lam_x = 1, lam_t = 1) 
}
\arguments{
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{Y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{basis}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{n_x}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{6}.}
\item{n_t}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{6}.}
\item{lam_x}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{lam_t}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
}
\description{
A step of the fgam_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{fitted}, \code{residuals}, \code{coefficients}, \code{intercept}, \code{surface}, \code{surface_x}, \code{edf}, \code{r_squared}, \code{n_x}, \code{n_t}, \code{lam_x}, \code{lam_t}, \code{linear_deviation}, \code{n}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_fgam_functional_gam(V, V)
}
