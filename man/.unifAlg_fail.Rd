% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.unifAlg_fail}
\alias{.unifAlg_fail}
\title{.unifAlg_fail}
\usage{
.unifAlg_fail(sub, why, oc)
}
\arguments{
\item{sub}{See Usage.}
\item{why}{See Usage.}
\item{oc}{See Usage.}
}
\description{
Part of the unifAlg_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{unified}, \code{mgu}, \code{reason}, \code{occurs_check}, \code{partial}, \code{n_bindings}, \code{method}.
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
