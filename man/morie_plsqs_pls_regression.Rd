% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_plsqs_pls_regression}
\alias{morie_plsqs_pls_regression}
\title{morie_plsqs_pls_regression}
\usage{
morie_plsqs_pls_regression(X, Y, n_components = 2)
}
\arguments{
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{Y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{n_components}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{2}.}
}
\description{
A step of the plsqs_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{coefficients}, \code{intercept}, \code{fitted}, \code{residuals}, \code{scores}, \code{weights}, \code{loadings}, \code{y_loadings}, \code{explained_x}, \code{explained_y}, \code{n_components}, \code{r_squared}, \code{n}, \code{p}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_plsqs_pls_regression(V, V)
}
