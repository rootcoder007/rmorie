% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.infmer_select_queries}
\alias{.infmer_select_queries}
\title{.infmer_select_queries}
\usage{
.infmer_select_queries(q, k, c = 5)
}
\arguments{
\item{q}{A matrix; passed to \code{nrow}.}
\item{k}{Passed to \code{.infmer_sparsity_measure}.}
\item{c}{Numeric; combined arithmetically in the body. Defaults to \code{5}.}
}
\description{
A step of the infmer_native implementation. Called by \code{morie_infmer}. See the file header for the source the module follows.
}
\value{
A vector, from \code{sort}.
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
