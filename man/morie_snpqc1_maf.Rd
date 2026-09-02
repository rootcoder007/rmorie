% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_snpqc1_maf}
\alias{morie_snpqc1_maf}
\title{Minor allele frequency per SNP, over non-missing calls}
\usage{
morie_snpqc1_maf(genotypes) 
}
\arguments{
\item{genotypes}{Passed to \code{.snpqc1_check}.}
}
\description{
A step of the snpqc1_native implementation. Called by \code{morie_snpqc1}, \code{morie_snpqc1_sex_check}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
S <- c("a", "b", "c")
morie_snpqc1_maf(S)
}
