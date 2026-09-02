% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MPursuit}
\alias{MPursuit}
\title{MPursuit}
\usage{
MPursuit(x, dictionary = NULL, natoms = 20, tol = 1e-10, decaystop = NULL) 
}
\arguments{
\item{x}{A vector; its length is taken.}
\item{dictionary}{Optional; may be \code{NULL}. Passed to \code{.morie_bx_mat}.}
\item{natoms}{Numeric; passed to \code{min}. Defaults to \code{20}.}
\item{tol}{Passed to \code{<=}. Defaults to \code{1e-10}.}
\item{decaystop}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the rangayyan_class2 implementation. Called by \code{VagTfd}. See the file header for the source the module follows.
}
\value{
A list with \code{coefficients}, \code{atoms}, \code{indices}, \code{residual}, \code{reconstruction}, \code{decay}, \code{energyratio}, \code{parameters}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
MPursuit(V)
}
