% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_permutations}
\alias{.morie_permutations}
\title{.morie_permutations}
\usage{
.morie_permutations(n)
}
\arguments{
\item{n}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
A step of the gibbons_native implementation. Called by \code{morie_rank_exact_null}, \code{morie_runs_updown}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
