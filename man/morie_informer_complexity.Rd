% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_informer_complexity}
\alias{morie_informer_complexity}
\title{morie_informer_complexity}
\usage{
morie_informer_complexity(L_Q, L_K, factor = 5)
}
\arguments{
\item{L_Q}{See Usage.}
\item{L_K}{See Usage.}
\item{factor}{Defaults to \code{5}.}
}
\description{
Part of the informer_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{full}, \code{probsparse}, \code{u}, \code{ratio}, \code{memory_full}, \code{memory_probsparse}.
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
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_informer_complexity(V, V)
}
