% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_rnn_visual_attention}
\alias{morie_geron_rnn_visual_attention}
\title{RNN visual attention: additive (Bahdanau-style) context over a feature map (Geron Ch 16, hmrvat)}
\usage{
morie_geron_rnn_visual_attention(features, h, W, U, v)
}
\arguments{
\item{features}{Spatial map (H, W, D) or (N, D).}
\item{h}{Decoder state.}
\item{W,U,v}{Attention parameters.}
}
\description{
RNN visual attention: additive (Bahdanau-style) context over a feature map (Geron Ch 16, hmrvat)
}
\value{
A list with \code{context}, \code{alpha}, \code{alpha_map}, \code{scores}, \code{entropy}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
morie_geron_rnn_visual_attention(features = c(1, 2, 3, 4, 5, 6, 7, 8), h = 0.5, W = c(1, 2, 3, 4, 5, 6, 7, 8), U = c(1, 2, 3, 4, 5, 6, 7, 8), v = c(1, 2, 3, 4, 5, 6, 7, 8))
}
