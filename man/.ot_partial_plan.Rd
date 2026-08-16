% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ot_partial_plan}
\alias{.ot_partial_plan}
\title{Partial transport of exactly m units, via a zero-price dummy row and}
\usage{
.ot_partial_plan(a, b, C, m)
}
\arguments{
\item{a}{A vector; its length is taken.}
\item{b}{A vector; its length is taken.}
\item{C}{Numeric; passed to \code{max}.}
\item{m}{Numeric; combined arithmetically in the body.}
}
\description{
column (Caffarelli and McCann 2010): the inequality-constrained problem becomes an ordinary balanced one and is solved exactly.
}
\value{
A list with \code{T}, \code{cost}.
}
\keyword{internal}
\examples{
.ot_partial_plan(a = c(1, 2, 3, 4, 5, 6, 7, 8), b = 5L, C = c(1, 2, 3, 4, 5, 6, 7, 8), m = 5L)
}
