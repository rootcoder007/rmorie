% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_reverse_autodiff}
\alias{morie_geron_reverse_autodiff}
\title{Reverse-mode automatic differentiation, delegated to morie_geron_autograd (Geron App A, hmrad)}
\usage{
morie_geron_reverse_autodiff(f, x)
}
\arguments{
\item{f}{Function `f(vars) -> tape node` (built from the supplied leaves).}
\item{x}{Point.}
}
\description{
Reverse-mode automatic differentiation, delegated to morie_geron_autograd (Geron App A, hmrad)
}
\value{
A list with \code{gradient}, \code{grad}, \code{value}, \code{tape_size}, \code{n_passes}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
