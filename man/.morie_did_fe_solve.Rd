% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_did_fe_solve}
\alias{.morie_did_fe_solve}
\title{Solve additive unit/time FEs on (possibly unbalanced) untreated}
\usage{
.morie_did_fe_solve(y0, u0, t0, iters = 50L)
}
\arguments{
\item{y0}{Numeric; combined arithmetically in the body.}
\item{u0}{Passed to \code{nlevels}.}
\item{t0}{Passed to \code{nlevels}.}
\item{iters}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{50L}.}
}
\description{
cells by alternating projections; returns per-level lookups.
}
\value{
A list with \code{a}, \code{g}.
}
\keyword{internal}
\examples{
.morie_did_fe_solve(y0 = c(1, 2, 3, 4, 5, 6, 7, 8), u0 = c(1, 2, 3, 4, 5, 6, 7, 8), t0 = c(1, 2, 3, 4, 5, 6, 7, 8))
}
