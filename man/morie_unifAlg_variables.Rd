% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_unifAlg_variables}
\alias{morie_unifAlg_variables}
\title{morie_unifAlg_variables}
\usage{
morie_unifAlg_variables(t)
}
\arguments{
\item{t}{Passed to \code{.unifAlg_check}.}
}
\description{
A step of the unifAlg_native implementation. Called by \code{.trmRew_canonical}, \code{morie_trmRew_lpo_greater}, \code{morie_trmRew_rule} and 1 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{$}.
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
x <- morie_unifAlg_var("x")
t <- morie_unifAlg_app("f", x, morie_unifAlg_var("y"))
morie_unifAlg_variables(t)
}
