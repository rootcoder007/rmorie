% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_cf_grow}
\alias{.morie_cf_grow}
\title{Grow one honest tree. `split_rows` chooses the splits; `est_rows`}
\usage{
.morie_cf_grow(X, y, d, split_rows, est_rows, depth, max_depth, min_leaf, mtry, imbalance_penalty = 0)
}
\arguments{
\item{X}{See Usage.}
\item{y}{See Usage.}
\item{d}{See Usage.}
\item{split_rows}{See Usage.}
\item{est_rows}{See Usage.}
\item{depth}{See Usage.}
\item{max_depth}{See Usage.}
\item{min_leaf}{See Usage.}
\item{mtry}{See Usage.}
\item{imbalance_penalty}{Defaults to \code{0}.}
}
\description{
(never seen by the splitter) fills the leaf values.
}
\value{
The value of \code{node}, as built in the body.
}
\keyword{internal}
