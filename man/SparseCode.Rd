% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{SparseCode}
\alias{SparseCode}
\title{SparseCode}
\usage{
SparseCode(x, D, sparsity = NULL, lam = NULL, maxiter = 2000, tol = 1e-10)
}
\arguments{
\item{x}{A vector; its length is taken.}
\item{D}{Passed to \code{.morie_bx_mat}.}
\item{sparsity}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{lam}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{maxiter}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{2000}.}
\item{tol}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1e-10}.}
}
\description{
A step of the rangayyan_class2 implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{alpha}, \code{support}, \code{reconstruction}, \code{residual}, \code{error}, \code{energyratio}, \code{mode}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(7)
D <- matrix(rnorm(16 * 8), 16, 8)
D <- sweep(D, 1, sqrt(rowSums(D^2)), "/")
x <- D[3, ] * 1.5 - D[10, ] * 0.7
r <- SparseCode(x, D, sparsity = 2)
str(r, max.level = 1)
}
