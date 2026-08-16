% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_km2_tokens}
\alias{.morie_km2_tokens}
\title{.morie_km2_tokens}
\usage{
.morie_km2_tokens(Y)
}
\arguments{
\item{Y}{A vector; its length is taken.}
}
\description{
A step of the kamath_llm2_native implementation. Called by \code{.morie_km2_count_word}, \code{morie_kamath_ch6_demographic_representation}, \code{morie_kamath_ch6_stereotypical_assoc}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_km2_tokens(V)
}
