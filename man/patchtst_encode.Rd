% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{patchtst_encode}
\alias{patchtst_encode}
\title{patchtst_encode}
\usage{
patchtst_encode(X, patch_len, stride = NULL, normalise = TRUE) 
}
\arguments{
\item{X}{Passed to \code{.patcht_mat}.}
\item{patch_len}{Passed to \code{channel_independent_tokens}.}
\item{stride}{Passed to \code{channel_independent_tokens}.}
\item{normalise}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the patchT_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{tokens}, \code{D}, \code{n_patches}, \code{n_tokens_total}, \code{norm_stats}, \code{normalised}, \code{cost}, \code{method}.
}
\keyword{internal}
\examples{
patchtst_encode(X = c(1, 2, 3, 4, 5, 6, 7, 8), patch_len = 5L)
}
