% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_timesfm_input_patches}
\alias{morie_timesfm_input_patches}
\title{morie_timesfm_input_patches}
\usage{
morie_timesfm_input_patches(x, patch_len, pad_value = 0) 
}
\arguments{
\item{x}{Passed to \code{unlist}.}
\item{patch_len}{Coerced to integer by the body, with \code{as.integer}.}
\item{pad_value}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
}
\description{
A step of the timesfm_native implementation. Called by \code{morie_timesfm}. See the file header for the source the module follows.
}
\value{
A list with \code{patches}, \code{n_patches}, \code{patch_len}, \code{n_padded}, \code{L}, \code{note}.
}
\keyword{internal}
\examples{
morie_timesfm_input_patches(x = c(1, 2, 3, 4, 5, 6, 7, 8), patch_len = 5L)
}
