% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{g_formula_pooled}
\alias{g_formula_pooled}
\title{g_formula_pooled}
\usage{
g_formula_pooled(Q_final, weights = NULL)
}
\arguments{
\item{Q_final}{See Usage.}
\item{weights}{Defaults to \code{NULL}.}
}
\description{
Part of the tlclust_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{psi}, \code{parametrization}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
g_formula_pooled(V)
}
