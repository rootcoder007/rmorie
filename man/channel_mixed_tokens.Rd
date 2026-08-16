% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{channel_mixed_tokens}
\alias{channel_mixed_tokens}
\title{channel_mixed_tokens}
\usage{
channel_mixed_tokens(X, patch_len, stride = NULL)
}
\arguments{
\item{X}{Passed to \code{.patcht_mat}.}
\item{patch_len}{Passed to \code{patchify}.}
\item{stride}{Passed to \code{patchify}.}
}
\description{
A step of the patchT_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{tokens}, \code{n_patches}, \code{n_tokens_total}, \code{design}, \code{note}.
}
\keyword{internal}
\examples{
channel_mixed_tokens(X = c(1, 2, 3, 4, 5, 6, 7, 8), patch_len = 5L)
}
