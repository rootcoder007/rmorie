% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_w4d_sae_forward}
\alias{.morie_w4d_sae_forward}
\title{.morie_w4d_sae_forward}
\usage{
.morie_w4d_sae_forward(A, Ws, bs, cs)
}
\arguments{
\item{A}{See Usage.}
\item{Ws}{A vector; its length is taken and its elements indexed.}
\item{bs}{A vector; indexed elementwise.}
\item{cs}{A vector; indexed elementwise.}
}
\description{
A step of the geron_w4d_native implementation. Called by \code{morie_geron_stacked_autoencoder_modules}. See the file header for the source the module follows.
}
\value{
A list with \code{hs}, \code{rs}.
}
\keyword{internal}
