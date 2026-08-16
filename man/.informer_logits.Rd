% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.informer_logits}
\alias{.informer_logits}
\title{.informer_logits}
\usage{
.informer_logits(q, K, scale)
}
\arguments{
\item{q}{Numeric; combined arithmetically in the body.}
\item{K}{Iterated over elementwise, with \code{sapply}.}
\item{scale}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the informer_native implementation. Called by \code{morie_informer_full_attention}, \code{morie_informer_probsparse_attention}, \code{morie_informer_sparsity_measure}. See the file header for the source the module follows.
}
\value{
A vector, from \code{sapply}.
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
