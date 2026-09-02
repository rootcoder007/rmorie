% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_varqc1_mixture}
\alias{morie_varqc1_mixture}
\title{EM for a Gaussian mixture, returning Cholesky factors}
\usage{
morie_varqc1_mixture(X, n_components = 2L, n_iter = 50L, seed = 1, covariance = "full", 
    min_variance = 1e-06, jitter = 1e-08, shrinkage = 0.05) 
}
\arguments{
\item{X}{Training matrix.}
\item{n_components}{Number of components.}
\item{n_iter}{EM iterations.}
\item{seed}{Seed for the generator shared with the Python arm.}
\item{covariance}{"full" or "diagonal".}
\item{min_variance}{Floor on each variance.}
\item{jitter}{Added to the diagonal.}
\item{shrinkage}{Each component covariance is pulled towards the   diagonal of the whole training set's covariance by this fraction.   That is not cosmetic: with a handful of training variants in seven   dimensions the per-component covariance is very nearly singular,   its Cholesky has a diagonal entry near zero, and the quadratic form   blows up -- turning a VQSLOD, a log RATIO that should sit in single   digits, into something of order 1e9 and letting the last bits of   the fit decide the answer. An absolute variance floor does not   help, because it is a floor in the annotation's own units and every   annotation has a different scale. Shrinking towards the data's own   diagonal is scale-free and is the standard remedy.}
}
\description{
Initialised by assigning point i to component i mod K -- a deterministic split, not a random one, so the fit does not move with the seed unless the seed is actually used. The seed is retained because a jittered restart is the usual escape from a degenerate component and it must be reproducible when it happens.
}
\value{
A list with the weights, means, Cholesky factors and the   log-likelihood trace.
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
morie_varqc1_mixture(X = VCF)
}
