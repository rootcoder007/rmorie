% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.infmer_full_attention}
\alias{.infmer_full_attention}
\title{.infmer_full_attention}
\usage{
.infmer_full_attention(q, k, v)
}
\arguments{
\item{q}{See Usage.}
\item{k}{See Usage.}
\item{v}{See Usage.}
}
\description{
Part of the infmer_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{output}, \code{attention}, \code{scores}.
}
\references{
----------
Zhou, H., Zhang, S., Peng, J., Zhang, S., Li, J., Xiong, H. & Zhang,
W. (2021) "Informer: Beyond Efficient Transformer for Long Sequence
Time-Series Forecasting", Proceedings of the AAAI Conference on
Artificial Intelligence 35(12), 11106-11115, arXiv:2012.07436.

See Also
--------
:mod:`morie.fn.informer` -- the implementation.
}
\keyword{internal}
