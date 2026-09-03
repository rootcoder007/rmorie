% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_snpqc1_ibd_moments}
\alias{morie_snpqc1_ibd_moments}
\title{PLINK\'s method-of-moments IBD estimates for every pair. Returns}
\usage{
morie_snpqc1_ibd_moments(genotypes, correction = TRUE) 
}
\arguments{
\item{genotypes}{Passed to \code{.snpqc1_check}.}
\item{correction}{Passed to \code{morie_snpqc1_ibs_given_ibd}. Defaults to \code{TRUE}.}
}
\description{
list(Z, pihat) where Z[[i]][[k]] is c(P(Z=0), P(Z=1), P(Z=2)) after the paper\'s bounding rules and pihat[i, k] = P(Z=2) + P(Z=1)/2.
}
\value{
A list with \code{Z}, \code{pihat}.
}
\keyword{internal}
\examples{
if (morie_crypto_sodium_available()) {
  morie_snpqc1_ibd_moments(genotypes = list(a = 1, b = 2))
}
}
