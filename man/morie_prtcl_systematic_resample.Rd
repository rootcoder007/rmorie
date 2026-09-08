% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_prtcl_systematic_resample}
\alias{morie_prtcl_systematic_resample}
\title{morie_prtcl_systematic_resample}
\usage{
morie_prtcl_systematic_resample(weights, u = NULL, e = NULL) 
}
\arguments{
\item{weights}{A vector; its length is taken.}
\item{u}{Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.}
\item{e}{Optional; may be \code{NULL}. Passed to \code{.ghc_unif}.}
}
\description{
A step of the prtcl_native implementation. Called by \code{morie_prtcl_particle_filter}. See the file header for the source the module follows.
}
\value{
The value of \code{idx}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_prtcl_systematic_resample(V)
}
