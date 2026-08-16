% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_informer_probsparse_attention}
\alias{morie_informer_probsparse_attention}
\title{morie_informer_probsparse_attention}
\usage{
morie_informer_probsparse_attention(Q, K, V, factor = 5, measure = "maxmean", n_sample = NULL, seed = 0, scale = NULL)
}
\arguments{
\item{Q}{See Usage.}
\item{K}{See Usage.}
\item{V}{See Usage.}
\item{factor}{Defaults to \code{5}.}
\item{measure}{Defaults to \code{"maxmean"}.}
\item{n_sample}{Defaults to \code{NULL}.}
\item{seed}{Defaults to \code{0}.}
\item{scale}{Defaults to \code{NULL}.}
}
\description{
Part of the informer_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{output}, \code{selected}, \code{u}, \code{L_Q}, \code{L_K}, \code{measure}, \code{complexity}, \code{method}, \code{note}.
}
\references{
----------
Zhou, H., Zhang, S., Peng, J., Zhang, S., Li, J., Xiong, H. & Zhang,
W. (2021) "Informer: Beyond Efficient Transformer for Long Sequence
Time-Series Forecasting", Proceedings of the AAAI Conference on
Artificial Intelligence 35(12), 11106-11115, arXiv:2012.07436.
Vaswani, A. et al. (2017) "Attention is all you need", Advances in
Neural Information Processing Systems 30, arXiv:1706.03762.
}
\keyword{internal}
