% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_km2_weat_sums}
\alias{.morie_km2_weat_sums}
\title{.morie_km2_weat_sums}
\usage{
.morie_km2_weat_sums(A_1, A_2, W_1, W_2)
}
\arguments{
\item{A_1}{A matrix; passed to \code{as.matrix}.}
\item{A_2}{A matrix; passed to \code{as.matrix}.}
\item{W_1}{See Usage.}
\item{W_2}{See Usage.}
}
\description{
A step of the kamath_llm2_native implementation. Called by \code{morie_kamath_ch6_weat_effect_size}, \code{morie_kamath_ch6_weat_function}. See the file header for the source the module follows.
}
\value{
The value of \code{list}.
}
\keyword{internal}
\examples{
.morie_km2_weat_sums(A_1 = c(1, 2, 3, 4, 5, 6, 7, 8), A_2 = c(1, 2, 3, 4, 5, 6, 7, 8), W_1 = c(1, 2, 3, 4, 5, 6, 7, 8), W_2 = c(1, 2, 3, 4, 5, 6, 7, 8))
}
