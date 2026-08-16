% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_bx_lstsq}
\alias{.morie_bx_lstsq}
\title{.morie_bx_lstsq}
\usage{
.morie_bx_lstsq(A, y, ridge = 1e-10)
}
\arguments{
\item{A}{A matrix; passed to \code{t}.}
\item{y}{Passed to \code{.morie_bx_mv}.}
\item{ridge}{Numeric; combined arithmetically in the body. Defaults to \code{1e-10}.}
}
\description{
A step of the rangayyan_class2 implementation. Called by \code{.morie_bx_omp}, \code{Lstm}, \code{Rbfn}. See the file header for the source the module follows.
}
\value{
The value of \code{.morie_bx_solve}.
}
\keyword{internal}
\examples{
set.seed(1)
A <- cbind(1, rnorm(10))
y <- A \%*\% c(0.5, 1.2) + rnorm(10, 0, 0.1)
.morie_bx_lstsq(A, as.numeric(y))
}
