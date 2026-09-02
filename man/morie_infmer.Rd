% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_infmer}
\alias{morie_infmer}
\title{Main entry point: morie_infmer}
\usage{
morie_infmer(q, k, v, c = 5) 
}
\arguments{
\item{q}{A matrix; indexed by row and column.}
\item{k}{A matrix; passed to \code{nrow}.}
\item{v}{A matrix; passed to \code{ncol}.}
\item{c}{Passed to \code{.infmer_complexity}. Defaults to \code{5}.}
}
\description{
Informer\'s ProbSparse self-attention (Zhou et al. 2021). q: numeric matrix [L_Q, d] (queries) k: numeric matrix [L_K, d] (keys) v: numeric matrix [L_K, d_v] (values) c: numeric sparsity constant (default 5) Returns named list: output : [L_Q, d_v] attended values selected_queries : 1-based integer vector of top-u query indices n_selected : integer length of that vector sparsity_scores : M(q_i, K) for every query complexity : full vs probsparse flop accounting
}
\value{
A list with \code{output}, \code{selected_queries}, \code{n_selected}, \code{sparsity_scores}, \code{complexity}.
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
morie_infmer(q = 0.5, k = 5L, v = c(1, 2, 3, 4, 5, 6, 7, 8))
}
