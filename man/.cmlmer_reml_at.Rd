% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.cmlmer_reml_at}
\alias{.cmlmer_reml_at}
\title{.cmlmer_reml_at}
\usage{
.cmlmer_reml_at(logdelta, Vk, y, X)
}
\arguments{
\item{logdelta}{Numeric; passed to \code{exp}.}
\item{Vk}{See Usage.}
\item{y}{A vector; its length is taken.}
\item{X}{A matrix; indexed by row and column.}
}
\description{
A step of the cmlmer_native implementation. Called by \code{morie_cmlmer_compressed_lmm}. See the file header for the source the module follows.
}
\value{
A list with \code{ll}, \code{delta}, \code{beta}, \code{s2g}, \code{L}.
}
\keyword{internal}
