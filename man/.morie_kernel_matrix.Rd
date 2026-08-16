% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_kernel_matrix}
\alias{.morie_kernel_matrix}
\title{.morie_kernel_matrix}
\usage{
.morie_kernel_matrix(X, Z = NULL, kernel = "rbf", gamma = NULL, degree = 3, coef0 = 1)
}
\arguments{
\item{X}{A matrix; passed to \code{ncol}.}
\item{Z}{Optional; may be \code{NULL}. A matrix; passed to \code{ncol}.}
\item{kernel}{One of \code{"linear"}, \code{"poly"}, \code{"rbf"}, \code{"sigmoid"}. Defaults to \code{"rbf"}.}
\item{gamma}{Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.}
\item{degree}{Numeric; combined arithmetically in the body. Defaults to \code{3}.}
\item{coef0}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
}
\description{
A step of the esl_native2 implementation. Called by \code{morie_esl_svc}, \code{morie_esl_svm_kernel}. See the file header for the source the module follows.
}
\value{
Nothing; this branch always raises.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_kernel_matrix(V)
}
