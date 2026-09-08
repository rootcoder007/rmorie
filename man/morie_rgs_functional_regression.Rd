% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_rgs_functional_regression}
\alias{morie_rgs_functional_regression}
\title{morie_rgs_functional_regression}
\usage{
morie_rgs_functional_regression(X, Y, basis = NULL) 
}
\arguments{
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{Y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{basis}{Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.}
}
\description{
A step of the rgs_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{beta}, \code{fitted}, \code{residuals}, \code{k}, \code{eigenvalues}, \code{explained}, \code{scores}, \code{mean_curve}, \code{r_squared}, \code{n}, \code{n_grid}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_rgs_functional_regression(V, V)
}
