% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_t2_simplify}
\alias{.morie_t2_simplify}
\title{.morie_t2_simplify}
\usage{
.morie_t2_simplify(clauses, lit)
}
\arguments{
\item{clauses}{See Usage.}
\item{lit}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the tail2_t04 implementation. Called by \code{Dpll}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
.morie_t2_simplify(clauses = c(1, 2, 3, 4, 5, 6, 7, 8), lit = 5L)
}
