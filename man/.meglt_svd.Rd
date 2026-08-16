% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.meglt_svd}
\alias{.meglt_svd}
\title{.meglt_svd}
\usage{
.meglt_svd(M)
}
\arguments{
\item{M}{A matrix; passed to \code{nrow}.}
}
\description{
A step of the meglt_native implementation. Called by \code{coherence}, \code{nuclear_norm}, \code{svt}. See the file header for the source the module follows.
}
\value{
A list with \code{U}, \code{s}, \code{Vt}.
}
\keyword{internal}
