% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.unifAlg_apply_once}
\alias{.unifAlg_apply_once}
\title{.unifAlg_apply_once}
\usage{
.unifAlg_apply_once(t, subst)
}
\arguments{
\item{t}{A vector; indexed elementwise.}
\item{subst}{A vector; indexed elementwise.}
}
\description{
A step of the unifAlg_native implementation. Called by \code{morie_unifAlg_apply_subst}, \code{morie_unifAlg_compose}, \code{morie_unifAlg_substitute}. See the file header for the source the module follows.
}
\value{
The value of \code{list}.
}
\references{
----------
Robinson, J. A. (1965) "A Machine-Oriented Logic Based on the
Resolution Principle", *Journal of the ACM* 12(1), 23-41,
doi:10.1145/321250.321253. Sec. 5 (the Unification Theorem: any
unifiable set has a most general unifier), the disagreement-set
algorithm, the occurs check, and the factorisation
:math:`\theta = \delta \circ \sigma` reproduced above.
}
\keyword{internal}
