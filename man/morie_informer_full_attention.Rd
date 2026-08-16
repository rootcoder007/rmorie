% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_informer_full_attention}
\alias{morie_informer_full_attention}
\title{morie_informer_full_attention}
\usage{
morie_informer_full_attention(Q, K, V, scale = NULL)
}
\arguments{
\item{Q}{Passed to \code{.informer_to_rows}.}
\item{K}{Passed to \code{.informer_to_rows}.}
\item{V}{Passed to \code{.informer_to_rows}.}
\item{scale}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the informer_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
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
\examples{
set.seed(2)
Q <- matrix(rnorm(16), 4, 4)
K <- matrix(rnorm(16), 4, 4)
V <- matrix(rnorm(16), 4, 4)
str(morie_informer_full_attention(Q, K, V), max.level = 1)
}
