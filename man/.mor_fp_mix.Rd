% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_fp_mix}
\alias{.mor_fp_mix}
\title{.mor_fp_mix}
\usage{
.mor_fp_mix(h, v)
}
\arguments{
\item{h}{Numeric; combined arithmetically in the body.}
\item{v}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the ecfp4_native implementation. Called by \code{.mor_fp_invariants}, \code{.mor_fp_morgan}, \code{morie_rdkfp}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.mor_fp_mix(V, V)
}
