% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_snpqc1_pihat_matrix}
\alias{morie_snpqc1_pihat_matrix}
\title{Just the pi-hat = P(Z=2) + P(Z=1)/2 matrix}
\usage{
morie_snpqc1_pihat_matrix(genotypes, correction = TRUE) 
}
\arguments{
\item{genotypes}{Passed to \code{morie_snpqc1_ibd_moments}.}
\item{correction}{Passed to \code{morie_snpqc1_ibd_moments}. Defaults to \code{TRUE}.}
}
\description{
A step of the snpqc1_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{$}.
}
\keyword{internal}
\examples{
morie_snpqc1_pihat_matrix(genotypes = list(a = 1, b = 2))
}
