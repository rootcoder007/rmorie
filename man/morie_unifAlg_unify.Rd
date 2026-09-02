% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_unifAlg_unify}
\alias{morie_unifAlg_unify}
\title{morie_unifAlg_unify}
\usage{
morie_unifAlg_unify(t1, t2, occurs_check = TRUE) 
}
\arguments{
\item{t1}{Passed to \code{.unifAlg_check}.}
\item{t2}{Passed to \code{.unifAlg_check}.}
\item{occurs_check}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the unifAlg_native implementation. Called by \code{.trmRew_overlap}. See the file header for the source the module follows.
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
\eqn{\theta = \delta \circ \sigma} reproduced above.
}
\keyword{internal}
\examples{
a <- morie_unifAlg_app("f", morie_unifAlg_var("x"), morie_unifAlg_const("b"))
b <- morie_unifAlg_app("f", morie_unifAlg_const("a"), morie_unifAlg_var("y"))
r <- morie_unifAlg_unify(a, b)
c(r$unified, r$n_bindings)
}
