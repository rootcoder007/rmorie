% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gwr_aicc}
\alias{.gwr_aicc}
\title{.gwr_aicc}
\usage{
.gwr_aicc(n, sigma2, tr_S)
}
\arguments{
\item{n}{Numeric; combined arithmetically in the body.}
\item{sigma2}{Numeric; passed to \code{log}.}
\item{tr_S}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the gwrcal_native implementation. Called by \code{.gwr_criterion}, \code{morie_gwrcal}, \code{morie_gwrcal_global_aicc}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
