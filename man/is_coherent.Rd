% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{is_coherent}
\alias{is_coherent}
\title{is_coherent}
\usage{
is_coherent(y, S, tol = 1e-09)
}
\arguments{
\item{y}{A vector; indexed elementwise.}
\item{S}{A matrix; passed to \code{nrow}.}
\item{tol}{Passed to \code{<=}. Defaults to \code{1e-09}.}
}
\description{
A step of the hierF_native implementation. Called by \code{mint_reconcile}. See the file header for the source the module follows.
}
\value{
A logical value.
}
\keyword{internal}
\examples{
S <- summing_matrix(list(c(0L, 1L)), 2L)
b <- c(1, 2)
is_coherent(c(sum(b), b), S)
}
