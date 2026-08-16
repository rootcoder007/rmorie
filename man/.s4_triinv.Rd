% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s4_triinv}
\alias{.s4_triinv}
\title{# Upper-triangular inverse by back substitution. Floors the pivot}
\usage{
.s4_triinv(R, p)
}
\arguments{
\item{R}{A matrix; indexed by row and column.}
\item{p}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
rather # than testing a condition number, so a rank-deficient design gives the # same large numbers in both arms instead of one raising.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
