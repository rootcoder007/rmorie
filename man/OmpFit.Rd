% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{OmpFit}
\alias{OmpFit}
\title{NOT from Rangayyan: Section 9.3 gives plain matching pursuit, not the}
\usage{
OmpFit(x, D, sparsity = NULL, tol = 1e-10)
}
\arguments{
\item{x}{See Usage.}
\item{D}{See Usage.}
\item{sparsity}{Defaults to \code{NULL}.}
\item{tol}{Defaults to \code{1e-10}.}
}
\description{
orthogonalised variant.  Pati, Rezaiifar and Krishnaprasad, Proc. 27th Asilomar Conf., pp. 40-44, 1993.  Re-solving the whole active set after every pick keeps the residue orthogonal to it, so each atom is chosen at most once and k atoms give the best k-term fit on that support.
}
\value{
A list with \code{coefficients}, \code{support}, \code{reconstruction}, \code{residual}, \code{error}, \code{energyratio}, \code{method}.
}
\keyword{internal}
