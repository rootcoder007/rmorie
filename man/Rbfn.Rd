% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Rbfn}
\alias{Rbfn}
\title{Rbfn}
\usage{
Rbfn(X, y, ncenters = NULL, spread = 1, centers = NULL, ridge = 1e-08, 
    query = NULL) 
}
\arguments{
\item{X}{Passed to \code{.morie_bx_mat}.}
\item{y}{Passed to \code{.morie_bx_vec}.}
\item{ncenters}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{spread}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{centers}{Optional; may be \code{NULL}. Passed to \code{.morie_bx_mat}.}
\item{ridge}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1e-08}.}
\item{query}{Optional; may be \code{NULL}. Passed to \code{.morie_bx_mat}.}
}
\description{
A step of the rangayyan_class2 implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{centers}, \code{weights}, \code{bias}, \code{predictions}, \code{queryvalues}, \code{mse}, \code{spread}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Rbfn(V, V)
}
