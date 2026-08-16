% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_unifAlg_var}
\alias{morie_unifAlg_var}
\title{morie_unifAlg_var}
\usage{
morie_unifAlg_var(name)
}
\arguments{
\item{name}{Coerced to character by the body, with \code{as.character}.}
}
\description{
A step of the unifAlg_native implementation. Called by \code{.trmRew_canonical}, \code{.trmRew_rename}, \code{morie_unifAlg_factor_through}. See the file header for the source the module follows.
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
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_unifAlg_var(V)
}
