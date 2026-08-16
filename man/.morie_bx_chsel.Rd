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
\item{trials}{Passed to \code{.morie_bx_mat}.}
\item{nselect}{A count; the body uses it as \code{seq_len(...)}.}
\item{rank}{A count; the body uses it as \code{matrix(...)}.}
\item{maxiter}{Passed to \code{.morie_bx_nmfmu}.}
\item{tol}{Passed to \code{.morie_bx_nmfmu}.}
\item{seed}{Passed to \code{.morie_bx_nmfmu}.}
}
\description{
A step of the rangayyan_class2 implementation. Called by \code{BciChSel}, \code{NmfChSel}. See the file header for the source the module follows.
}
\value{
A list with \code{X}, \code{C}, \code{W}, \code{H}, \code{error}, \code{rmsd}, \code{normalized}, \code{ranking}, \code{selected}.
}
\keyword{internal}
