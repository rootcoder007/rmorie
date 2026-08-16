% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{NmfChSel}
\alias{NmfChSel}
\title{NmfChSel}
\usage{
NmfChSel(trials, nselect, rank = 4, maxiter = 200, tol = 1e-08, seed = 1)
}
\arguments{
\item{trials}{Passed to \code{.morie_bx_chsel}.}
\item{nselect}{Passed to \code{.morie_bx_chsel}.}
\item{rank}{Passed to \code{.morie_bx_chsel}. Defaults to \code{4}.}
\item{maxiter}{Passed to \code{.morie_bx_chsel}. Defaults to \code{200}.}
\item{tol}{Passed to \code{.morie_bx_chsel}. Defaults to \code{1e-08}.}
\item{seed}{Passed to \code{.morie_bx_chsel}. Defaults to \code{1}.}
}
\description{
A step of the rangayyan_class2 implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{selected}, \code{rmsd}, \code{ranking}, \code{normalized}, \code{W}, \code{H}, \code{covariance}, \code{error}, \code{method}.
}
\keyword{internal}
