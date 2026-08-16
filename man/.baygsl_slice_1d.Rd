% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.baygsl_slice_1d}
\alias{.baygsl_slice_1d}
\title{One univariate slice transition. Returns list(x, n_eval, interval)}
\usage{
.baygsl_slice_1d(logf, x0, e, w = 1, max_steps = 50L, lower = .baygsl_NEG_INF, upper = .baygsl_POS_INF)
}
\arguments{
\item{logf}{See Usage.}
\item{x0}{Numeric; combined arithmetically in the body.}
\item{e}{Passed to \code{.baygsl_expo}.}
\item{w}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{max_steps}{Numeric; combined arithmetically in the body. Defaults to \code{50L}.}
\item{lower}{Defaults to \code{.baygsl_NEG_INF}.}
\item{upper}{Defaults to \code{.baygsl_POS_INF}.}
}
\description{
`logf` is the log of an unnormalised density; rng is the .ghc_rng environment so the chain stays reproducible.
}
\value{
Nothing; this branch always raises.
}
\keyword{internal}
