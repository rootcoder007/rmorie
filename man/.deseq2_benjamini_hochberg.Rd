% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.deseq2_benjamini_hochberg}
\alias{.deseq2_benjamini_hochberg}
\title{Benjamini-Hochberg step-up adjusted p-values}
\usage{
.deseq2_benjamini_hochberg(p)
}
\arguments{
\item{p}{Numeric vector of raw p-values.}
}
\description{
Benjamini-Hochberg step-up adjusted p-values
}
\value{
Numeric vector of adjusted p-values.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.deseq2_benjamini_hochberg(V)
}
