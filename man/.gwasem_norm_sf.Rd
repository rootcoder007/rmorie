% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gwasem_norm_sf}
\alias{.gwasem_norm_sf}
\title{.gwasem_norm_sf}
\usage{
.gwasem_norm_sf(z)
}
\arguments{
\item{z}{Numeric; passed to \code{abs}.}
}
\description{
A step of the gwasem_native implementation. Called by \code{morie_gwasem}. See the file header for the source the module follows.
}
\value{
The value of \code{pnorm}.
}
\keyword{internal}
