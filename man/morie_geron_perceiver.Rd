% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_perceiver}
\alias{morie_geron_perceiver}
\title{Perceiver: iterated latent-to-input scaled dot-product cross-attention (Geron Ch 16, hmprcv)}
\usage{
morie_geron_perceiver(x, latents, n_iter = 2, W_q = NULL, W_k = NULL, W_v = NULL)
}
\arguments{
\item{x}{Input (N, D).}
\item{latents}{Learned latents (L, D_lat).}
\item{n_iter}{Cross-attention rounds.}
\item{W_q,W_k,W_v}{Optional projections.}
}
\description{
Perceiver: iterated latent-to-input scaled dot-product cross-attention (Geron Ch 16, hmprcv)
}
\value{
A list with \code{latents}, \code{attention}, \code{attention_cost}, \code{self_attention_cost}, \code{n_iter}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
