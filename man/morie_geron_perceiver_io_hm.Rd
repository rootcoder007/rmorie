% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_perceiver_io_hm}
\alias{morie_geron_perceiver_io_hm}
\title{Perceiver IO: adds a cross-attention output decoder (Geron Ch 16, hmprio)}
\usage{
morie_geron_perceiver_io_hm(x, latents, queries, n_iter = 2, W_q = NULL, W_k = NULL, W_v = NULL)
}
\arguments{
\item{x,latents,queries}{Input, latents and output queries.}
\item{n_iter,W_q,W_k,W_v}{Encoder controls.}
}
\description{
Perceiver IO: adds a cross-attention output decoder (Geron Ch 16, hmprio)
}
\value{
A list with \code{outputs}, \code{decoder_attention}, \code{latents}, \code{encoder_attention}, \code{decoder_cost}, \code{encoder_cost}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
