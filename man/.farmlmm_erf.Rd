% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.farmlmm_erf}
\alias{.farmlmm_erf}
\title{Base R has no erf/erfc; both are pnorm in disguise. Defined here so}
\usage{
.farmlmm_erf(x)
}
\arguments{
\item{x}{Numeric; combined arithmetically in the body.}
}
\description{
the arm stays base-R only, as the package requires.
}
\value{
A numeric value.
}
\references{
Liu, X., Huang, M., Fan, B., Buckler, E. S. & Zhang, Z. (2016)
"Iterative Usage of Fixed and Random Effect Models for Powerful and
Efficient Genome-Wide Association Studies", PLoS Genetics 12(2),
e1005767, doi:10.1371/journal.pgen.1005767.
Yu, J. et al. (2006) "A unified mixed-model method for association
mapping that accounts for multiple levels of relatedness", Nature
Genetics 38(2), 203-208.
Segura, V. et al. (2012) "An efficient multi-locus mixed-model
approach for genome-wide association studies in structured
populations", Nature Genetics 44(7), 825-830.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.farmlmm_erf(V)
}
