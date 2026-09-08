% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{VagFeat}
\alias{VagFeat}
\title{VagFeat: statistical vibroarthrogram characterisation}
\usage{
VagFeat(vag, fs, n_segments = 8) 
}
\arguments{
\item{vag}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{fs}{Numeric; combined arithmetically in the body.}
\item{n_segments}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{8}.}
}
\description{
A step of the rangayyan_phys implementation. Called by \code{VagKnee}. See the file header for the source the module follows.
}
\value{
A list with \code{mean}, \code{variance}, \code{skewness}, \code{kurtosis}, \code{kurtosis_excess}, \code{form_factor}, \code{mobility}, \code{activity}, \code{turns_count}, \code{turns_per_second}, \code{var_of_segment_means}, \code{var_of_segment_ms}, \code{segment_means}, \code{segment_ms}, \code{entropy_bits}, \code{rms}, \code{duration_s}, \code{fs_hz}, \code{n_segments}, \code{units}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(6)
fs <- 2000
vag <- rnorm(fs, 0, seq(0.5, 2, length.out = fs))
r <- VagFeat(vag, fs = fs)
str(r, max.level = 1)
}
