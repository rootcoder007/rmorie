% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_snpqc1_ld_prune}
\alias{morie_snpqc1_ld_prune}
\title{Window-based pruning: drop one of any pair with r^2 above the}
\usage{
morie_snpqc1_ld_prune(genotypes, window = 50, step = 5, r2 = 0.2)
}
\arguments{
\item{genotypes}{See Usage.}
\item{window}{Defaults to \code{50}.}
\item{step}{Defaults to \code{5}.}
\item{r2}{Defaults to \code{0.2}.}
}
\description{
threshold. Returns kept SNP indices (1-based).
}
\value{
The value of \code{keep}, as built in the body.
}
\keyword{internal}
\examples{
S <- c("a", "b", "c")
morie_snpqc1_ld_prune(S)
}
