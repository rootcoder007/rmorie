% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.hrz3_ade}
\alias{.hrz3_ade}
\title{Density-weighted average derivative, Horowitz Sec. 2.6.1:}
\usage{
.hrz3_ade(X, y, h)
}
\arguments{
\item{X}{A matrix; indexed by row and column.}
\item{y}{A vector; its length is taken and its elements indexed.}
\item{h}{Numeric; combined arithmetically in the body.}
}
\description{
delta = E[f_X(X) dE(Y|X)/dX] = -2 E[f_X\\'(X) Y]. In a single-index model delta is proportional to beta, so it fixes the index DIRECTION without optimising over it.  The leave-one-out form is used: the own-observation term of a kernel density derivative is identically zero only in the limit, and keeping it biases delta toward zero.
}
\value{
The value of \code{delta}, as built in the body.
}
\keyword{internal}
\examples{
.hrz3_ade(X = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), h = 0.5)
}
