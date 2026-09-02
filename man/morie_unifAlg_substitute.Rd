% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_unifAlg_substitute}
\alias{morie_unifAlg_substitute}
\title{morie_unifAlg_substitute}
\usage{
morie_unifAlg_substitute(t, subst) 
}
\arguments{
\item{t}{Passed to \code{.unifAlg_check}.}
\item{subst}{Passed to \code{.unifAlg_apply_once}.}
}
\description{
A step of the unifAlg_native implementation. Called by \code{.trmRew_canonical}, \code{morie_trmRew_rewrite_step}. See the file header for the source the module follows.
}
\value{
The value of \code{.unifAlg_apply_once}.
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
x <- morie_unifAlg_var("x")
t <- morie_unifAlg_app("f", x)
morie_unifAlg_substitute(t, list(x = morie_unifAlg_const("a")))
}
