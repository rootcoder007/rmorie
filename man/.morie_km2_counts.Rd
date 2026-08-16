% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_km2_counts}
\alias{.morie_km2_counts}
\title{.morie_km2_counts}
\usage{
.morie_km2_counts(keys)
}
\arguments{
\item{keys}{A vector; its length is taken.}
}
\description{
A step of the kamath_llm2_native implementation. Called by \code{morie_kamath_bleu_score}, \code{morie_kamath_ch8_rouge_n}, \code{morie_kamath_rouge_n}. See the file header for the source the module follows.
}
\value{
The value of \code{stats::setNames}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_km2_counts(V)
}
