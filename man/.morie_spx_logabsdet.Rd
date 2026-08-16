% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_spx_logabsdet}
\alias{.morie_spx_logabsdet}
\title{(sign, log|det|) by LU with partial pivoting}
\usage{
.morie_spx_logabsdet(A)
}
\arguments{
\item{A}{A matrix; passed to \code{nrow}.}
}
\description{
A step of the sp_fill implementation. Called by \code{.morie_spx_sarneg2}. See the file header for the source the module follows.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
