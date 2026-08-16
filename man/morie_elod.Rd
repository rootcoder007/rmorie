% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_elod}
\alias{morie_elod}
\title{Expected LOD per progeny}
\usage{
morie_elod(var_qtl, var_residual)
}
\arguments{
\item{var_qtl}{QTL variance, non-negative.}
\item{var_residual}{Residual variance, strictly positive.}
}
\description{
Equations (5a)-(5c) of Lander & Botstein (1989): the exact expected LOD and the small-effect Taylor approximation, with the gap between them rather than one silently substituted for the other.
}
\value{
A named list with keys \code{elod}, \code{approximation},   \code{gap}, \code{ratio}, \code{note}.
}
\keyword{internal}
