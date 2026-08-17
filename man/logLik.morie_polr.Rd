% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{logLik.morie_polr}
\alias{logLik.morie_polr}
\title{Log-likelihood of method for \code{morie_polr} objects}
\usage{
logLik.morie_polr(object, ...)
}
\arguments{
\item{object}{A \code{morie_polr} object.}
\item{...}{Ignored; accepted for S3 consistency.}
}
\description{
Log-likelihood of method for \code{morie_polr} objects
}
\value{
The value of \code{val}, as built in the body.
}
\keyword{internal}
\examples{
set.seed(1)
x <- rnorm(80)
y <- factor(cut(1.5 * x + rnorm(80), 3), ordered = TRUE)
fit <- morie_polr(y ~ x, data = data.frame(y, x))
ll <- logLik(fit)
inherits(ll, "logLik")
}
