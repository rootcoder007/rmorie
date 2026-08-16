% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.kinship_from_markers}
\alias{.kinship_from_markers}
\title{.kinship_from_markers}
\usage{
.kinship_from_markers(G, markers = NULL)
}
\arguments{
\item{G}{Passed to \code{.farmlmm_to_mat}.}
\item{markers}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the farmlmm_native implementation. Called by \code{.random_effect_step}. See the file header for the source the module follows.
}
\value{
A list with \code{K}, \code{markers_used}, \code{n_markers}, \code{all_markers}.
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
