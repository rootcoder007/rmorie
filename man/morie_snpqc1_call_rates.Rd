% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_snpqc1_call_rates}
\alias{morie_snpqc1_call_rates}
\title{Per-SNP and per-individual call rates}
\usage{
morie_snpqc1_call_rates(genotypes) 
}
\arguments{
\item{genotypes}{Passed to \code{.snpqc1_check}.}
}
\description{
A step of the snpqc1_native implementation. Called by \code{morie_snpqc1}. See the file header for the source the module follows.
}
\value{
A list with \code{per_snp}, \code{per_ind}.
}
\keyword{internal}
\examples{
S <- c("a", "b", "c")
morie_snpqc1_call_rates(S)
}
