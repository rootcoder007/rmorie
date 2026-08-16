% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_timesfm_causal_mask}
\alias{morie_timesfm_causal_mask}
\title{morie_timesfm_causal_mask}
\usage{
morie_timesfm_causal_mask(n_patches)
}
\arguments{
\item{n_patches}{See Usage.}
}
\description{
A step of the timesfm_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{mask}, \code{n_patches}, \code{training_signals}.
}
\keyword{internal}
\examples{
morie_timesfm_causal_mask(n_patches = 5L)
}
