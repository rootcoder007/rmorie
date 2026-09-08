% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{SeizDict}
\alias{SeizDict}
\title{SeizDict}
\usage{
SeizDict(signals, labels, iterations = 7, atoms = NULL, test = NULL) 
}
\arguments{
\item{signals}{Passed to \code{.morie_bx_mat}.}
\item{labels}{Passed to \code{.morie_bx_vec}.}
\item{iterations}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{7}.}
\item{atoms}{Optional; may be \code{NULL}. Passed to \code{.morie_bx_mat}.}
\item{test}{Optional; may be \code{NULL}. Passed to \code{.morie_bx_mat}.}
}
\description{
A step of the rangayyan_class2 implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{dictionary}, \code{coefficients}, \code{error}, \code{predictions}, \code{isseizure}, \code{testclass}, \code{accuracy}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(3)
S <- rbind(matrix(rnorm(4 * 64, 0, 1), 4, 64),
           matrix(rnorm(4 * 64, 0, 3), 4, 64))
y <- rep(c(0L, 1L), each = 4)
r <- SeizDict(S, y, iterations = 3)
str(r, max.level = 1)
}
