% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_unifAlg_match}
\alias{morie_unifAlg_match}
\title{morie_unifAlg_match}
\usage{
morie_unifAlg_match(pattern, subject)
}
\arguments{
\item{pattern}{Passed to \code{.unifAlg_check}.}
\item{subject}{Passed to \code{.unifAlg_check}.}
}
\description{
A step of the unifAlg_native implementation. Called by \code{morie_trmRew_rewrite_step}, \code{morie_unifAlg_factor_through}. See the file header for the source the module follows.
}
\value{
The value of \code{sub}, as built in the body.
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
pat <- morie_unifAlg_app("f", morie_unifAlg_var("x"))
sub <- morie_unifAlg_app("f", morie_unifAlg_const("a"))
r <- morie_unifAlg_match(pat, sub)
str(r, max.level = 1)
}
