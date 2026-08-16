% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.farmcpu}
\alias{.farmcpu}
\title{.farmcpu}
\usage{
.farmcpu(y, G, max_iter = 10L, threshold = NULL, seed = 0L)
}
\arguments{
\item{y}{Passed to \code{.to_vec}.}
\item{G}{Passed to \code{.farmlmm_to_mat}.}
\item{max_iter}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{10L}.}
\item{threshold}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{seed}{Accepted by the signature and not used anywhere in the body. Defaults to \code{0L}.}
}
\description{
A step of the farmlmm_native implementation. Called by \code{morie_farmlmm}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{selected}, \code{p}, \code{iterations}, \code{converged}, \code{oscillating}, \code{threshold}, \code{history}, \code{method}, \code{note}.
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
