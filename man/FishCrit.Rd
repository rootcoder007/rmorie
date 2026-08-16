% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{FishCrit}
\alias{FishCrit}
\title{J = (m1 - m2)^2 / (s1^2 + s2^2).  Close kin to eq (10.112) but NOT}
\usage{
FishCrit(x1, x2)
}
\arguments{
\item{x1}{See Usage.}
\item{x2}{See Usage.}
}
\description{
the same measure: that divides |m1 - m2| by (s1 + s2).  They rank features identically only when the dispersions are equal.
}
\value{
A list with \code{j}, \code{means}, \code{variances}, \code{normalized_distance}, \code{agrees_with_eq_10_112_ranking_only_for_equal_spread}, \code{is_not_eq_10_112}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
FishCrit(V, V)
}
