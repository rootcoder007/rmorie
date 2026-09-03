% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_unifAlg_factor_through}
\alias{morie_unifAlg_factor_through}
\title{morie_unifAlg_factor_through}
\usage{
morie_unifAlg_factor_through(general, other, over) 
}
\arguments{
\item{general}{Passed to \code{morie_unifAlg_apply_subst}.}
\item{other}{Passed to \code{morie_unifAlg_apply_subst}.}
\item{over}{See Usage.}
}
\description{
A step of the unifAlg_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{delta}, as built in the body.
}
\references{
----------
Robinson, J. A. (1965) "A Machine-Oriented Logic Based on the
Resolution Principle", *Journal of the ACM* 12(1), 23-41,
doi:10.1145/321250.321253. Sec. 5 (the Unification Theorem: any
unifiable set has a most general unifier), the disagreement-set
algorithm, the occurs check, and the factorisation
\eqn{\theta = \delta \circ \sigma} reproduced above.
}
\keyword{internal}
\examples{
morie_unifAlg_factor_through(general = c(1, 2, 3, 4, 5, 6, 7, 8),
  other = c(1, 2, 3, 4, 5, 6, 7, 8), over = c(1, 2, 3, 4, 5, 6, 7, 8))
}
