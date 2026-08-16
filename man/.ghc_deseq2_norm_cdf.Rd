% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ghc_deseq2_norm_cdf}
\alias{.ghc_deseq2_norm_cdf}
\title{.ghc_deseq2_norm_cdf}
\usage{
.ghc_deseq2_norm_cdf(z)
}
\arguments{
\item{z}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the deseq2_native implementation. Called by \code{.ghc_deseq2_norm_ppf}, \code{deseq2}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.ghc_deseq2_norm_cdf(V)
}
