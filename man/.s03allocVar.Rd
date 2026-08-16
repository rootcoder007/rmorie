% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s03allocVar}
\alias{.s03allocVar}
\title{Neyman (1934) eq. (37), p. 579, with sigma_i^2 eliminated:}
\usage{
.s03allocVar(M, S, m)
}
\arguments{
\item{M}{A vector; its length is taken and its elements indexed.}
\item{S}{A vector; indexed elementwise.}
\item{m}{A vector; indexed elementwise.}
}
\description{
sigma^2 = sum_i M_i (M_i - m_i) S_i^2 / m_i.
}
\value{
The value of \code{tot}, as built in the body.
}
\keyword{internal}
\examples{
.s03allocVar(M = 5L, S = c(1, 2, 3, 4, 5, 6, 7, 8), m = 5L)
}
