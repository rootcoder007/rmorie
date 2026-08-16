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
\item{X}{A matrix; indexed by row and column.}
\item{y}{A vector; indexed elementwise.}
\item{d}{A vector; indexed elementwise.}
\item{split_rows}{A vector; its length is taken and its elements indexed.}
\item{est_rows}{A vector; its length is taken and its elements indexed.}
\item{depth}{Numeric; combined arithmetically in the body.}
\item{max_depth}{Passed to \code{.morie_cf_grow}.}
\item{min_leaf}{Numeric; combined arithmetically in the body.}
\item{mtry}{Numeric; passed to \code{min}.}
\item{imbalance_penalty}{A flag; the body branches on it. Defaults to \code{0}.}
}
\description{
(never seen by the splitter) fills the leaf values.
}
\value{
The value of \code{node}, as built in the body.
}
\keyword{internal}
