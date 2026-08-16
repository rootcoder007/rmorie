% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ghc_survvae_unpack}
\alias{.ghc_survvae_unpack}
\title{Internal: unpack the flat parameter vector v = (W | bias |}
\usage{
.ghc_survvae_unpack(v, K, d)
}
\arguments{
\item{v}{See Usage.}
\item{K}{See Usage.}
\item{d}{See Usage.}
}
\description{
log_shapes | log_scales) into named pieces. Mirrors the Python arm\\\'s .unpack(): W is a list of K length-d vectors, bias is length K, shapes and scales are length K (positives, recovered via exp()).
}
\value{
A list with \code{W}, \code{bias}, \code{shapes}, \code{scales}.
}
\keyword{internal}
