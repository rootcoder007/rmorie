% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_groebn}
\alias{morie_groebn}
\title{morie_groebn}
\usage{
morie_groebn(polys, order = "lex", prune = TRUE, reduced = TRUE)
}
\arguments{
\item{polys}{Iterated over elementwise, with \code{lapply}.}
\item{order}{Passed to \code{.groebn_buchberger}. Defaults to \code{"lex"}.}
\item{prune}{Passed to \code{.groebn_buchberger}. Defaults to \code{TRUE}.}
\item{reduced}{Passed to \code{.groebn_buchberger}. Defaults to \code{TRUE}.}
}
\description{
Entry point ---------------------------------------------------------------------------
}
\value{
The value of \code{.groebn_buchberger}.
}
\keyword{internal}
