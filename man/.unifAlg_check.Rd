% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.unifAlg_check}
\alias{.unifAlg_check}
\title{.unifAlg_check}
\usage{
.unifAlg_check(t)
}
\arguments{
\item{t}{A vector; its length is taken and its elements indexed.}
}
\description{
A step of the unifAlg_native implementation. Called by \code{.unifAlg_apply_once}, \code{morie_unifAlg_apply_subst}, \code{morie_unifAlg_compose} and 5 others in the module. See the file header for the source the module follows.
}
\value{
Nothing; this branch always raises.
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
