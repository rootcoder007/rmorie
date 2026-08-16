% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_gwasem}
\alias{morie_gwasem}
\title{EMMAX GWAS}
\usage{
morie_gwasem(y, genotypes, kinship = NULL, covariates = NULL, trait = "quantitative", test = "f", ml = FALSE, per_marker_reml = FALSE, min_maf = 0)
}
\arguments{
\item{y}{Phenotype vector.}
\item{genotypes}{n x m minor-allele count matrix.}
\item{kinship}{Optional n x n relatedness matrix; IBS when omitted.}
\item{covariates}{Optional n x q covariate matrix.}
\item{trait}{\code{"quantitative"} or \code{"binary"}.}
\item{test}{\code{"f"} or \code{"score"}.}
\item{ml}{Use ML rather than REML.}
\item{per_marker_reml}{Re-estimate variance components per marker.}
\item{min_maf}{Skip markers below this MAF.}
}
\description{
Three-step procedure: Gower-normalise \code{S} (eq.5), estimate \code{sigma_a^2, sigma_e^2} once under the null (eq.6) and test each marker with the fixed variance (eq.7). Case-control is the 0/1 response as a quantitative trait, in the spirit of Armitage.
}
\value{
A list with the per-marker statistics, the variance   components, the genomic-control inflation factor and the   skipped-marker indices.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_gwasem(V, V)
}
