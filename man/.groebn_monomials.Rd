% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.groebn_monomials}
\alias{.groebn_monomials}
\title{.groebn_monomials}
\usage{
.groebn_monomials(f, order = "lex")
}
\arguments{
\item{f}{Passed to \code{names}.}
\item{order}{Passed to \code{.groebn_key}. Defaults to \code{"lex"}.}
}
\description{
A step of the groebn_native implementation. Called by \code{.groebn_leading_monomial}. See the file header for the source the module follows.
}
\value{
The value of \code{[}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.groebn_monomials(V)
}
