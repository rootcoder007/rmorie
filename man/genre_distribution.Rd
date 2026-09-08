% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{genre_distribution}
\alias{genre_distribution}
\alias{morie_caltbR}
\title{Weighted average of \eqn{p(g \mid i)} over a set of items}
\usage{
genre_distribution(items, p_g_given_i, weights = NULL) 

morie_caltbR(items, p_g_given_i, weights = NULL) 
}
\arguments{
\item{items}{Integer vector of item indices (0-based in Python,   1-based in R -- shifted internally).}
\item{p_g_given_i}{Numeric matrix or list of rows with one row per   item and one column per genre, holding \eqn{p(g \mid i)}.}
\item{weights}{Optional numeric vector of per-item weights; if   \code{NULL} each item contributes 1.}
}
\description{
Implements eqs. (2)-(3): a per-genre average of the genre memberships, weighted either by user-supplied weights (recency of the play, say) or by rank (when called from the reranker with a rank_weights argument).
}
\value{
Numeric vector \eqn{q(g)} of length G, the weighted   average.
}
\keyword{internal}
\examples{
genre_distribution(items = list(a = 1, b = 2), p_g_given_i = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2))
}
