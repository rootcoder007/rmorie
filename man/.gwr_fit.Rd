% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gwr_fit}
\alias{.gwr_fit}
\title{.gwr_fit}
\usage{
.gwr_fit(y, X, D, bw, kernel, adaptive)
}
\arguments{
\item{y}{A matrix; passed to \code{crossprod}.}
\item{X}{A matrix; indexed by row and column.}
\item{D}{A matrix; indexed by row and column.}
\item{bw}{Passed to \code{.gwr_kernel}.}
\item{kernel}{Passed to \code{.gwr_kernel}.}
\item{adaptive}{A flag; the body branches on it.}
}
\description{
A step of the gwrcal_native implementation. Called by \code{.gwr_criterion}, \code{morie_gwrcal}. See the file header for the source the module follows.
}
\value{
A list with \code{params}, \code{se_params}, \code{fitted}, \code{resid}, \code{tr_S}, \code{sigma2}, \code{edf_resid}, \code{n_rank_deficient}.
}
\keyword{internal}
