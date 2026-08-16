% Generated from the module source by ledger/gen_rd.R -- roxygen2 hangs
% on this package. Usage comes from args(), so it tracks the formals.
\name{morie_alfesf_esmfold_confidence}
\alias{morie_alfesf_esmfold_confidence}
\alias{morie_alfesf}
\title{ESMFold/AlphaFold confidence: decode, run, fit, calibrate}
\usage{
morie_alfesf_esmfold_confidence(lddt_logits = NULL, pae_logits = NULL, features = NULL, weights = NULL, lddt = NULL, chain_id = NULL, temperature = 1, l2 = 0.001, iters = 300L, lr = 0.5, pae_bin_width = 0.5)
}
\description{
R arm of alfesf -- ESMFold/AlphaFold confidence: decode, run, fit, calibrate.   binned expectation) and 1.9.7 (pTM, d0 = 1.24 (N-15)^(1/3) - 1.8).
}
\value{
A list with components \code{temperature}, \code{estimate}, \code{plddt}, \code{plddt_mean}, \code{ptm}, \code{iptm}, \code{pae}, \code{d0}, \code{weights}, \code{W}, \code{b}, \code{route}, \code{n_lddt_bins}, \code{n_pae_bins}, \code{method}, \code{note}.
}
\references{
Lin, Z. et al. (2023) Science 379(6637), 1123-1130, doi:10.1126/science.ade2574.
Jumper, J. et al. (2021) Nature 596(7873), 583-589, SI 1.9.6 (pLDDT as a
Zhang, Y. & Skolnick, J. (2004) Proteins 57(4), 702-710 -- TM score, d0.
Guo, C. et al. (2017) ICML 70, 1321-1330 -- temperature scaling.
}
\keyword{internal}
