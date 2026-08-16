% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_informer_sparsity_measure}
\alias{morie_informer_sparsity_measure}
\title{morie_informer_sparsity_measure}
\usage{
morie_informer_sparsity_measure(q, K, measure = "exact", scale = NULL)
}
\arguments{
\item{q}{See Usage.}
\item{K}{Passed to \code{.informer_to_rows}.}
\item{measure}{Compared against \code{"maxmean"}. Defaults to \code{"exact"}.}
\item{scale}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the informer_native implementation. Called by \code{morie_informer_kl_from_uniform}, \code{morie_informer_select_queries}. See the file header for the source the module follows.
}
\value{
A numeric value.
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
