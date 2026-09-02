% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{g_formula_sequential}
\alias{g_formula_sequential}
\title{g_formula_sequential}
\usage{
g_formula_sequential(Q_seq) 
}
\arguments{
\item{Q_seq}{A vector; its length is taken and its elements indexed.}
}
\description{
A step of the tlclust_native implementation. Called by \code{morie_tlclust}. See the file header for the source the module follows.
}
\value{
A list with \code{psi}, \code{parametrization}, \code{T}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
g_formula_sequential(V)
}
