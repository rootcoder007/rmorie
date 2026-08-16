% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.groebn_leading_coeff}
\alias{.groebn_leading_coeff}
\title{.groebn_leading_coeff}
\usage{
.groebn_leading_coeff(f, order = "lex")
}
\arguments{
\item{f}{A vector; indexed elementwise.}
\item{order}{Passed to \code{.groebn_leading_monomial}. Defaults to \code{"lex"}.}
}
\description{
A step of the groebn_native implementation. Called by \code{.groebn_reduce_basis}. See the file header for the source the module follows.
}
\value{
The value of \code{[[}.
}
\keyword{internal}
