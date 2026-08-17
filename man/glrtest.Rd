% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{glrtest}
\alias{glrtest}
\title{glrtest}
\usage{
glrtest(x, p0, p1, threshold = NULL, family = "bernoulli", sd = 1)
}
\arguments{
\item{x}{Passed to \code{morie_glr_test}.}
\item{p0}{Passed to \code{morie_glr_test}.}
\item{p1}{Passed to \code{morie_glr_test}.}
\item{threshold}{Passed to \code{morie_glr_test}.}
\item{family}{Passed to \code{morie_glr_test}. Defaults to \code{"bernoulli"}.}
\item{sd}{Passed to \code{morie_glr_test}. Defaults to \code{1}.}
}
\description{
A step of the glm_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{morie_glr_test}.
}
\keyword{internal}
\examples{
set.seed(2)
x <- c(rpois(40, 1), rpois(40, 4))
res <- glrtest(x, p0 = 1, p1 = 4, family = "poisson")
res$changepoint >= 0
}
