% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_rlhfRS_ndcg}
\alias{morie_rlhfRS_ndcg}
\title{Discounted cumulative gain over its own best possible value}
\usage{
morie_rlhfRS_ndcg(relevance, k) 
}
\arguments{
\item{relevance}{The graded relevance of the ranked list.}
\item{k}{The cut-off.}
}
\description{
The discount is one over the log base two of the rank plus one, so the first position is undiscounted and the tenth is worth about a third of it. Dividing by the ideal ordering's gain makes it one for a perfect ranking and comparable across queries with different numbers of relevant items. A query with nothing relevant has an ideal gain of zero, so the ratio is undefined; it is reported as zero, and that is a choice, not an accident of the arithmetic.
}
\value{
A number between zero and one.
}
\references{
  Zhao, X., Zhang, L., Ding, Z., Xia, L., Tang, J. and Yin, D. (2018)
    "Recommendations with negative feedback via pairwise deep
    reinforcement learning." KDD, 1040-1048.
    doi:10.1145/3219819.3219886.
  Horvitz, D.G. and Thompson, D.J. (1952) "A generalization of
    sampling without replacement from a finite universe." JASA
    47(260), 663-685.
  Swaminathan, A. and Joachims, T. (2015) "The self-normalized
    estimator for counterfactual learning." NeurIPS 28, 3231-3239.
  Dudik, M., Langford, J. and Li, L. (2011) "Doubly robust policy
    evaluation and learning." ICML, 1097-1104.
  Jarvelin, K. and Kekalainen, J. (2002) "Cumulated gain-based
    evaluation of IR techniques." ACM TOIS 20(4), 422-446.
}
\keyword{internal}
\examples{
morie_rlhfRS_ndcg(relevance = c(1, 2, 3, 4, 5, 6, 7, 8), k = 5L)
}
