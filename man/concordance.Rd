% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{concordance}
\alias{concordance}
\title{concordance}
\usage{
concordance(imputed, truth) 
}
\arguments{
\item{imputed}{Passed to \code{.impfun_as_double_vec}.}
\item{truth}{Passed to \code{.impfun_as_double_vec}.}
}
\description{
A step of the impfun_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{concordance}, \code{mean_absolute_error}, \code{n}, \code{method}.
}
\references{
----------
Howie, B. N., Donnelly, P. & Marchini, J. (2009) "A Flexible and
Accurate Genotype Imputation Method for the Next Generation of
Genome-Wide Association Studies", *PLoS Genetics* 5(6), e1000529,
doi:10.1371/journal.pgen.1000529. The main innovation as a flexible
modelling framework that increases accuracy and combines information
across MULTIPLE REFERENCE PANELS while remaining computationally
feasible; higher accuracy than other methods when HapMap provides the
sole reference panel, with the panel size constraining the
improvements possible; greatly enhanced accuracy from expanding the
panel to thousands of chromosomes, outperforming other methods at both
rare and common SNPs with error rates 15-20\% lower than the closest
competitor; and the practical advantages of this approach to
integrating information across panels genotyped on different sets of
SNPs.

Li, N. & Stephens, M. (2003) "Modeling Linkage Disequilibrium and
Identifying Recombination Hotspots Using Single-Nucleotide
Polymorphism Data", *Genetics* 165(4), 2213-2233,
doi:10.1093/genetics/165.4.2213. The copying model.

Browning, S. R. & Browning, B. L. (2007) "Rapid and Accurate Haplotype
Phasing and Missing-Data Inference for Whole-Genome Association
Studies by Use of Localized Haplotype Clustering", *American Journal
of Human Genetics* 81(5), 1084-1097, doi:10.1086/521987.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
concordance(V, V)
}
