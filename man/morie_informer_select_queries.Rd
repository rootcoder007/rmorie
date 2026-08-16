% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_informer_select_queries}
\alias{morie_informer_select_queries}
\title{morie_informer_select_queries}
\usage{
morie_informer_select_queries(Q, K, factor = 5, measure = "maxmean", n_sample = NULL, seed = 0)
}
\arguments{
\item{Q}{See Usage.}
\item{K}{See Usage.}
\item{factor}{Defaults to \code{5}.}
\item{measure}{Defaults to \code{"maxmean"}.}
\item{n_sample}{Defaults to \code{NULL}.}
\item{seed}{Defaults to \code{0}.}
}
\description{
Part of the informer_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{top}, \code{u}, \code{scores}, \code{L_Q}, \code{L_K}, \code{n_sample}, \code{measure}.
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
