% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.infmer_kl_from_uniform}
\alias{.infmer_kl_from_uniform}
\title{.infmer_kl_from_uniform}
\usage{
.infmer_kl_from_uniform(p)
}
\arguments{
\item{p}{A vector; its length is taken.}
}
\description{
A step of the infmer_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{kl}, as built in the body.
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
