% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_km2_count_word}
\alias{.morie_km2_count_word}
\title{.morie_km2_count_word}
\usage{
.morie_km2_count_word(word, outputs)
}
\arguments{
\item{word}{Passed to \code{==}.}
\item{outputs}{Iterated over elementwise, with \code{vapply}.}
}
\description{
A step of the kamath_llm2_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_km2_count_word(V, V)
}
