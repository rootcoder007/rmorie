% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.groebn_leading_term}
\alias{.groebn_leading_term}
\title{.groebn_leading_term}
\usage{
.groebn_leading_term(f, order = "lex")
}
\arguments{
\item{f}{A vector; indexed elementwise.}
\item{order}{Passed to \code{.groebn_leading_monomial}. Defaults to \code{"lex"}.}
}
\description{
A step of the groebn_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.groebn_leading_term(V)
}
