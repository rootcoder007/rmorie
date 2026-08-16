% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.groebn_buchberger}
\alias{.groebn_buchberger}
\title{.groebn_buchberger}
\usage{
.groebn_buchberger(F, order = "lex", prune = TRUE, reduced = TRUE)
}
\arguments{
\item{F}{Iterated over elementwise, with \code{lapply}.}
\item{order}{Passed to \code{.groebn_key}. Defaults to \code{"lex"}.}
\item{prune}{A flag; the body branches on it. Defaults to \code{TRUE}.}
\item{reduced}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
Buchberger completion and reduce_basis ---------------------------------------------------------------------------
}
\value{
A list with \code{estimate}, \code{basis}, \code{order}, \code{size}, \code{reduced}, \code{n_pairs}, \code{n_reductions}, \code{n_skipped}, \code{pruned}, \code{method}.
}
\keyword{internal}
