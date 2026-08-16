% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.farmlmm_to_mat}
\alias{.farmlmm_to_mat}
\title{Mirror _s03core.mat}
\usage{
.farmlmm_to_mat(X)
}
\arguments{
\item{X}{A matrix; passed to \code{nrow}.}
}
\description{
A step of the farmlmm_native implementation. Called by \code{.confounding}, \code{.farmcpu}, \code{.farmlmm_wls} and 2 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{X}, as built in the body.
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
