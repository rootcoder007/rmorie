% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_varqc1}
\alias{morie_varqc1}
\title{Filter variant records by hard thresholds, by VQSR, or by both}
\usage{
morie_varqc1(vcf, thresholds = NULL, fields = NULL, mode = "snp", method = "hard", positive = NULL, negative = NULL, n_components = 2L, n_iter = 50L, seed = 1, covariance = "full", tranches = c(90, 99, 99.9, 100), vqsr_fields = NULL, min_variance = 1e-06, shrinkage = 0.05)
}
\arguments{
\item{vcf}{Annotation matrix, one row per variant.}
\item{thresholds}{Triples of annotation, direction and cutoff; the   GATK recommendations for the mode when omitted.}
\item{fields}{Column names. Required.}
\item{mode}{"snp" or "indel", which selects the default thresholds.}
\item{method}{"hard", "vqsr" or "both".}
\item{positive}{Row indices of the training-positive set (one-based).}
\item{negative}{Row indices of the training-negative set.}
\item{n_components}{Mixture components.}
\item{n_iter}{EM iterations.}
\item{seed}{Seed for the generator shared with the Python arm.}
\item{covariance}{"full" or "diagonal".}
\item{tranches}{Target sensitivities to the training-positive set.}
\item{vqsr_fields}{Which annotations the mixture uses.}
\item{min_variance}{Floor on each variance.}
\item{shrinkage}{Pulls each component covariance towards the diagonal   of the training set's own covariance, which is what keeps a   seven-dimensional fit on twenty variants from being singular.}
}
\description{
Filter variant records by hard thresholds, by VQSR, or by both
}
\value{
A list with per-record FILTER strings and counts, and for the   VQSR routes the VQSLOD score, the tranche and the fitted mixtures.
}
\references{
  Broad Institute, GATK Best Practices: "Hard-filtering germline short
    variants," the source of the threshold values above.
  DePristo, M.A. et al. (2011) "A framework for variation discovery
    and genotyping using next-generation DNA sequencing data." Nature
    Genetics 43(5), 491-498.
  Van der Auwera, G.A. et al. (2013) "From FastQ data to
    high-confidence variant calls." Current Protocols in
    Bioinformatics 43, 11.10.1-11.10.33.
  Dempster, A.P., Laird, N.M. and Rubin, D.B. (1977) JRSS B 39(1),
    1-38.
}
\keyword{internal}
\examples{
FIELDS <- c("QD", "QUAL", "SOR", "FS", "MQ", "MQRankSum", "ReadPosRankSum")
N <- 60L
VCF <- matrix(0, N, 7)
morie_varqc1(VCF, fields = FIELDS, mode = "snp")
}
