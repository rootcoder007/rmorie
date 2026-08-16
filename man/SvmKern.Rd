% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{SvmKern}
\alias{SvmKern}
\title{SvmKern}
\usage{
SvmKern(X, y, query = NULL, kernel = "rbf", gamma = NULL, degree = 3, coef0 = 0, C = 1, maxiter = 2000, tol = 1e-06)
}
\arguments{
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{query}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{kernel}{One of \code{"linear"}, \code{"poly"}, \code{"rbf"}, \code{"sigmoid"}. Defaults to \code{"rbf"}.}
\item{gamma}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{degree}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{3}.}
\item{coef0}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
\item{C}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
\item{maxiter}{Passed to \code{.morie_rg_smo}. Defaults to \code{2000}.}
\item{tol}{Passed to \code{.morie_rg_smo}. Defaults to \code{1e-06}.}
}
\description{
A step of the rangayyan_class implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
xor <- matrix(c(0, 0, 0, 1, 1, 0, 1, 1), ncol = 2, byrow = TRUE)
yx <- c(-1, 1, 1, -1)
SvmKern(xor, yx, kernel = "rbf", gamma = 1, C = 10)
}
