% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.fixed_effect_scan}
\alias{.fixed_effect_scan}
\title{.fixed_effect_scan}
\usage{
.fixed_effect_scan(y, G, covariates = integer(0), K = NULL)
}
\arguments{
\item{y}{See Usage.}
\item{G}{See Usage.}
\item{covariates}{Defaults to \code{integer(0)}.}
\item{K}{Defaults to \code{NULL}.}
}
\description{
Part of the farmlmm_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{p}, \code{beta}, \code{covariates}, \code{note}.
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
