% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_pmpfit_check}
\alias{morie_pmpfit_check}
\title{Check Pitman-Yor parameters}
\usage{
morie_pmpfit_check(alpha, theta)
}
\arguments{
\item{alpha}{Discount.}
\item{theta}{Concentration.}
}
\description{
Validates 0 <= alpha < 1 and theta > -alpha. The range is dictated by Proposition 4, not a convention.
}
\value{
A list with `alpha`, `theta`, `is_dirichlet`, `note`.
}
\keyword{internal}
