% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_bx_chsel}
\alias{.morie_bx_chsel}
\title{Shared core of BciChSel and NmfChSel: eqs (9.94)-(9.96)}
\usage{
.morie_bx_chsel(trials, nselect, rank, maxiter, tol, seed)
}
\arguments{
\item{trials}{See Usage.}
\item{nselect}{See Usage.}
\item{rank}{See Usage.}
\item{maxiter}{See Usage.}
\item{tol}{See Usage.}
\item{seed}{See Usage.}
}
\description{
Part of the rangayyan_class2 implementation; see the file header for the source it follows.
}
\value{
A list with \code{X}, \code{C}, \code{W}, \code{H}, \code{error}, \code{rmsd}, \code{normalized}, \code{ranking}, \code{selected}.
}
\keyword{internal}
