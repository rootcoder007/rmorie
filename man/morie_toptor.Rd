% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_toptor}
\alias{morie_toptor}
\alias{morie_topological_torsion}
\title{Topological torsions for a molecule, or for a set of molecules}
\usage{
morie_toptor(elements, bonds, reference = NULL, common_types = NULL, activities = NULL, 
    permutations = 40, seed = 0) 

morie_topological_torsion(elements, bonds, reference = NULL, common_types = NULL, activities = NULL, 
    permutations = 40, seed = 0) 
}
\arguments{
\item{elements}{Element symbols (or list thereof for many molecules).}
\item{bonds}{Bond list(s).}
\item{reference}{Optional \code{(elements, bonds)} probe.}
\item{common_types}{Atom types kept as themselves.}
\item{activities}{Optional activities for the trend vector.}
\item{permutations}{Number of randomisations.}
\item{seed}{Seed for the shared generator.}
}
\description{
Topological torsions for a molecule, or for a set of molecules
}
\value{
A list with \code{estimate}, \code{torsions},   \code{n_distinct}, \code{n_total}, \code{method}, and optionally   \code{reference_torsions}, \code{similarity}, \code{ranking},   \code{trend}.
}
\keyword{internal}
\examples{
elements <- c("C", "C", "C", "C")
bonds <- list(c(0, 1), c(1, 2), c(2, 3))
morie_toptor(elements, bonds)
}
