% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{attention_cost}
\alias{attention_cost}
\title{attention_cost}
\usage{
attention_cost(L, patch_len, stride = NULL, D = 1, channel_independent = TRUE) 
}
\arguments{
\item{L}{Coerced to integer by the body, with \code{as.integer}.}
\item{patch_len}{Coerced to integer by the body, with \code{as.integer}.}
\item{stride}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{D}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{1}.}
\item{channel_independent}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the patchT_native implementation. Called by \code{patchtst_encode}. See the file header for the source the module follows.
}
\value{
A list with \code{n_patches}, \code{pointwise}, \code{patched}, \code{reduction}, \code{stride}, \code{patch_len}, \code{note}.
}
\keyword{internal}
\examples{
attention_cost(L = 5L, patch_len = 5L)
}
