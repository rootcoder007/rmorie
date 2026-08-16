% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.infmer_complexity}
\alias{.infmer_complexity}
\title{.infmer_complexity}
\usage{
.infmer_complexity(L_Q, L_K, c = 5)
}
\arguments{
\item{L_Q}{See Usage.}
\item{L_K}{See Usage.}
\item{c}{Defaults to \code{5}.}
}
\description{
Part of the infmer_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{full_attention_flops}, \code{probsparse_flops}, \code{ratio}, \code{u}.
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
