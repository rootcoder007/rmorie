% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_w4d_sae_backward}
\alias{.morie_w4d_sae_backward}
\title{.morie_w4d_sae_backward}
\usage{
.morie_w4d_sae_backward(A, Ws, bs, cs, hs, rs)
}
\arguments{
\item{A}{A matrix; passed to \code{nrow}.}
\item{Ws}{A vector; its length is taken and its elements indexed.}
\item{bs}{Iterated over elementwise, with \code{lapply}.}
\item{cs}{Iterated over elementwise, with \code{lapply}.}
\item{hs}{A vector; indexed elementwise.}
\item{rs}{A vector; indexed elementwise.}
}
\description{
A step of the geron_w4d_native implementation. Called by \code{morie_geron_stacked_autoencoder_modules}. See the file header for the source the module follows.
}
\value{
A list with \code{dW}, \code{db}, \code{dc}.
}
\keyword{internal}
