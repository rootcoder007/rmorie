% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.infmer_sparsity_measure}
\alias{.infmer_sparsity_measure}
\title{.infmer_sparsity_measure}
\usage{
.infmer_sparsity_measure(q, k)
}
\arguments{
\item{q}{A matrix; passed to \code{ncol}.}
\item{k}{A matrix; passed to \code{ncol}.}
}
\description{
A step of the infmer_native implementation. Called by \code{.infmer_select_queries}, \code{morie_infmer}. See the file header for the source the module follows.
}
\value{
The value of \code{m_scores}, as built in the body.
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
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.infmer_sparsity_measure(V, V)
}
