% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_prtcl_effective_sample_size}
\alias{morie_prtcl_effective_sample_size}
\title{morie_prtcl_effective_sample_size}
\usage{
morie_prtcl_effective_sample_size(weights)
}
\arguments{
\item{weights}{Numeric; passed to \code{sum}.}
}
\description{
A step of the prtcl_native implementation. Called by \code{morie_prtcl_particle_filter}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_prtcl_effective_sample_size(V)
}
