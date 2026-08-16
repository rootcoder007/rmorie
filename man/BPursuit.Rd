% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{BPursuit}
\alias{BPursuit}
\title{NOT from Rangayyan: the book covers matching pursuit (9.3) and EMD}
\usage{
BPursuit(x, D, lam = 0.01, maxiter = 2000, tol = 1e-10)
}
\arguments{
\item{x}{See Usage.}
\item{D}{See Usage.}
\item{lam}{Defaults to \code{0.01}.}
\item{maxiter}{Defaults to \code{2000}.}
\item{tol}{Defaults to \code{1e-10}.}
}
\description{
dictionary learning (9.5), not basis pursuit.  Chen, Donoho and Saunders, SIAM J. Sci. Comput. 20(1):33-61, 1998 for the L1 formulation; Daubechies, Defrise and De Mol, CPAM 57(11):1413-1457, 2004 for the thresholded-Landweber solver.  Greedy pursuit fixes an atom the moment it is chosen; here every coefficient stays free.
}
\value{
A list with \code{alpha}, \code{support}, \code{reconstruction}, \code{residual}, \code{l1norm}, \code{objective}, \code{iterations}, \code{method}.
}
\keyword{internal}
