% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_fp_envkey}
\alias{.mor_fp_envkey}
\title{Canonical key of a bond set: sorted 0-based indices, four digits each}
\usage{
.mor_fp_envkey(bs)
}
\arguments{
\item{bs}{A vector; its length is taken.}
}
\description{
A step of the ecfp4_native implementation. Called by \code{.mor_fp_morgan}. See the file header for the source the module follows.
}
\value{
A character value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.mor_fp_envkey(V)
}
