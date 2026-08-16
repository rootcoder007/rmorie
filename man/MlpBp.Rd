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
\item{X}{Passed to \code{.morie_bx_mat}.}
\item{y}{Passed to \code{.morie_bx_vec}.}
\item{hidden}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{4}.}
\item{eta}{Numeric; combined arithmetically in the body. Defaults to \code{0.5}.}
\item{alpha}{Numeric; combined arithmetically in the body. Defaults to \code{0.9}.}
\item{maxiter}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{500}.}
\item{tol}{Passed to \code{<=}. Defaults to \code{1e-04}.}
\item{seed}{Passed to \code{.morie_bx_rng}. Defaults to \code{1}.}
}
\description{
A step of the rangayyan_class2 implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{weights}, \code{offsets}, \code{predictions}, \code{outputs}, \code{classes}, \code{accuracy}, \code{mse}, \code{iterations}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
MlpBp(V, V)
}
