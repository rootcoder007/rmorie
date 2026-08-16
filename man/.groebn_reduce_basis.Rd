% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.groebn_reduce_basis}
\alias{.groebn_reduce_basis}
\title{.groebn_reduce_basis}
\usage{
.groebn_reduce_basis(G, order = "lex")
}
\arguments{
\item{G}{See Usage.}
\item{order}{Passed to \code{.groebn_leading_monomial}. Defaults to \code{"lex"}.}
}
\description{
A step of the groebn_native implementation. Called by \code{.groebn_buchberger}. See the file header for the source the module follows.
}
\value{
The value of \code{[}.
}
\keyword{internal}
