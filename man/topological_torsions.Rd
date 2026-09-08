% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{topological_torsions}
\alias{topological_torsions}
\title{Every topological torsion in a molecule, with multiplicities}
\usage{
topological_torsions(elements, bonds, common_types = NULL) 
}
\arguments{
\item{elements}{Heavy-atom element symbols.}
\item{bonds}{Bonds as \code{(i, j)} or \code{(i, j, order)}.}
\item{common_types}{Atom types kept as themselves; the rest become   \code{"Y"}.}
}
\description{
Every topological torsion in a molecule, with multiplicities
}
\value{
Canonical torsion code -> count.
}
\keyword{internal}
\examples{
elements <- c("C", "C", "C", "C")
bonds <- list(c(0, 1), c(1, 2), c(2, 3))
topological_torsions(elements, bonds)
}
