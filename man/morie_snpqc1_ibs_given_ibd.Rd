% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_snpqc1_ibs_given_ibd}
\alias{morie_snpqc1_ibs_given_ibd}
\title{PLINK\'s Table 1: P(I | Z) for one SNP. Returns a 3x3 matrix with}
\usage{
morie_snpqc1_ibs_given_ibd(x_count, y_count, correction = TRUE) 
}
\arguments{
\item{x_count}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{y_count}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{correction}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
rows Z=0,1,2 and columns I=0,1,2 (lower triangle zero).
}
\value{
The value of \code{rbind}.
}
\keyword{internal}
\examples{
if (morie_crypto_sodium_available()) {
  morie_snpqc1_ibs_given_ibd(x_count = 5L, y_count = 5L)
}
}
