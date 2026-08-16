% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_varqc1_logpdf}
\alias{morie_varqc1_logpdf}
\title{log sum_k w_k N(x; mu_k, L_k L_k'), from the Cholesky factors}
\usage{
morie_varqc1_logpdf(x, weights, means, chols)
}
\arguments{
\item{x}{The point.}
\item{weights}{Mixture weights.}
\item{means}{Component means, a list of vectors.}
\item{chols}{Component Cholesky factors, a list of matrices.}
}
\description{
log sum_k w_k N(x; mu_k, L_k L_k'), from the Cholesky factors
}
\value{
The log density.
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
