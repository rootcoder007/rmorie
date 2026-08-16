% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.qsfrgr_grow}
\alias{.qsfrgr_grow}
\title{.qsfrgr_grow}
\usage{
.qsfrgr_grow(X, time, event, struct_rows, leaf_rows, feats_n, min_leaf, max_depth, depth, e, rule)
}
\arguments{
\item{X}{A matrix; indexed by row and column.}
\item{time}{Passed to \code{.qsfrgr_best_split}.}
\item{event}{Passed to \code{.qsfrgr_events_in}.}
\item{struct_rows}{A vector; its length is taken and its elements indexed.}
\item{leaf_rows}{A vector; indexed elementwise.}
\item{feats_n}{Numeric; passed to \code{min}.}
\item{min_leaf}{Numeric; combined arithmetically in the body.}
\item{max_depth}{Passed to \code{.qsfrgr_grow}.}
\item{depth}{Numeric; combined arithmetically in the body.}
\item{e}{Passed to \code{.ghc_unif}.}
\item{rule}{Passed to \code{.qsfrgr_best_split}.}
}
\description{
A step of the qsfrgr_native implementation. Called by \code{morie_qsfrgr_forest}. See the file header for the source the module follows.
}
\value{
A list with \code{leaf}, \code{f}, \code{thr}, \code{l}, \code{r}.
}
\references{
  Cui, Y., Kosorok, M.R., Sverdrup, E., Wager, S. and Zhu, R. (2023)
    "Estimating heterogeneous treatment effects with right-censored
    data via causal survival forests." Journal of the Royal
    Statistical Society Series B 85(2), 179-211.
    doi:10.1093/jrsssb/qkac001.
  Wager, S. and Athey, S. (2018) "Estimation and inference of
    heterogeneous treatment effects using random forests." Journal of
    the American Statistical Association 113(523), 1228-1242.
  Athey, S., Tibshirani, J. and Wager, S. (2019) "Generalized random
    forests." The Annals of Statistics 47(2), 1148-1178.
  Ishwaran, H., Kogalur, U.B., Blackstone, E.H. and Lauer, M.S. (2008)
    "Random survival forests." The Annals of Applied Statistics 2(3),
    841-860.
  Kaplan, E.L. and Meier, P. (1958) "Nonparametric estimation from
    incomplete observations." Journal of the American Statistical
    Association 53(282), 457-481.
  Meinshausen, N. (2006) "Quantile regression forests." Journal of
    Machine Learning Research 7, 983-999.
}
\keyword{internal}
