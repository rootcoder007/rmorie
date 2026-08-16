% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MlpBp}
\alias{MlpBp}
\title{MlpBp}
\usage{
MlpBp(X, y, hidden = 4, eta = 0.5, alpha = 0.9, maxiter = 500, tol = 1e-04, seed = 1)
}
\arguments{
\item{X}{See Usage.}
\item{y}{See Usage.}
\item{hidden}{Defaults to \code{4}.}
\item{eta}{Defaults to \code{0.5}.}
\item{alpha}{Defaults to \code{0.9}.}
\item{maxiter}{Defaults to \code{500}.}
\item{tol}{Defaults to \code{1e-04}.}
\item{seed}{Defaults to \code{1}.}
}
\description{
Part of the rangayyan_class2 implementation; see the file header for the source it follows.
}
\value{
A list with \code{weights}, \code{offsets}, \code{predictions}, \code{outputs}, \code{classes}, \code{accuracy}, \code{mse}, \code{iterations}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
MlpBp(V, V)
}
