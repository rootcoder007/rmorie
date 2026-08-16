% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s03unif}
\alias{.s03unif}
\title{.s03unif}
\usage{
.s03unif(n, base = 2L)
}
\arguments{
\item{n}{A count; the body uses it as \code{seq_len(...)}.}
\item{base}{Iterated over elementwise, with \code{vapply}. Defaults to \code{2L}.}
}
\description{
A step of the helpers_s03 implementation. Called by \code{.s03normdraws}. See the file header for the source the module follows.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.s03unif(V)
}
