% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ghc_deseq2_median}
\alias{.ghc_deseq2_median}
\title{.ghc_deseq2_median}
\usage{
.ghc_deseq2_median(v)
}
\arguments{
\item{v}{Numeric; passed to \code{sort}.}
}
\description{
A step of the deseq2_native implementation. Called by \code{.ghc_deseq2_mad}, \code{dispersion_trend}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.ghc_deseq2_median(V)
}
