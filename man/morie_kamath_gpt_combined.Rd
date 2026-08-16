% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_kamath_gpt_combined}
\alias{morie_kamath_gpt_combined}
\title{morie_kamath_gpt_combined}
\usage{
morie_kamath_gpt_combined(L_1, L_2, lam = 0.5)
}
\arguments{
\item{L_1,L_2,lam}{Objective values and weight.}
}
\description{
morie_kamath_gpt_combined
}
\value{
A list with \code{estimate}, \code{L1}, \code{L2}, \code{lambda}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_kamath_gpt_combined(V, V)
}
