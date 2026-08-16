% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ghc_svd}
\alias{.ghc_svd}
\title{.ghc_svd}
\usage{
.ghc_svd(A)
}
\arguments{
\item{A}{A matrix; passed to \code{nrow}.}
}
\description{
A step of the lsa_native implementation. Called by \code{lsa_decompose}. See the file header for the source the module follows.
}
\value{
A list with \code{T}, \code{S}, \code{Dt}.
}
\keyword{internal}
