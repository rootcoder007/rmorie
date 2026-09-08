% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{size_factors}
\alias{size_factors}
\title{Median-of-ratios size factors}
\usage{
size_factors(counts) 
}
\arguments{
\item{counts}{Gene-by-sample integer count matrix.}
}
\description{
The reference for each gene is its geometric mean across samples, and \code{s_j = median_i K_ij / K_i^R} over genes with a positive reference. Genes with any zero count drop out, which is what makes the estimator robust to genes present in only some samples.
}
\value{
Numeric vector of size factors, one per sample.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
size_factors(V)
}
