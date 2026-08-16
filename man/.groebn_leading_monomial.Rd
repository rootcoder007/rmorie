% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.groebn_leading_monomial}
\alias{.groebn_leading_monomial}
\title{.groebn_leading_monomial}
\usage{
.groebn_leading_monomial(f, order = "lex")
}
\arguments{
\item{f}{Passed to \code{.groebn_monomials}.}
\item{order}{Passed to \code{.groebn_monomials}. Defaults to \code{"lex"}.}
}
\description{
A step of the groebn_native implementation. Called by \code{.groebn_buchberger}, \code{.groebn_divide}, \code{.groebn_leading_coeff} and 3 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{[}.
}
\keyword{internal}
