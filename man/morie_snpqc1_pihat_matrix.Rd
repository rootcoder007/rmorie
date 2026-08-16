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
\item{genotypes}{See Usage.}
\item{correction}{Defaults to \code{TRUE}.}
}
\description{
Part of the snpqc1_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{$}.
}
\keyword{internal}
\examples{
morie_snpqc1_pihat_matrix(genotypes = list(a = 1, b = 2))
}
