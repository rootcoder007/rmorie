% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_unifAlg_occurs}
\alias{morie_unifAlg_occurs}
\title{morie_unifAlg_occurs}
\usage{
morie_unifAlg_occurs(name, t)
}
\arguments{
\item{name}{See Usage.}
\item{t}{Passed to \code{morie_unifAlg_variables}.}
}
\description{
A step of the unifAlg_native implementation. Called by \code{morie_unifAlg_unify}. See the file header for the source the module follows.
}
\value{
The value of \code{\%in\%}.
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
