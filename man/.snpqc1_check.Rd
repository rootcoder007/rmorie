% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.snpqc1_check}
\alias{.snpqc1_check}
\title{.snpqc1_check}
\usage{
.snpqc1_check(genotypes)
}
\arguments{
\item{genotypes}{A matrix; passed to \code{as.matrix}.}
}
\description{
A step of the snpqc1_native implementation. Called by \code{morie_snpqc1}, \code{morie_snpqc1_call_rates}, \code{morie_snpqc1_heterozygosity} and 5 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{G}, \code{n}, \code{m}.
}
\keyword{internal}
