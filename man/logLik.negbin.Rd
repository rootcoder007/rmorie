% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{logLik.negbin}
\alias{logLik.negbin}
\title{Log-likelihood of method for \code{negbin} objects}
\usage{
logLik.negbin(object, ...)
}
\arguments{
\item{object}{A \code{negbin} object.}
\item{...}{Ignored; accepted for S3 consistency.}
}
\description{
Log-likelihood of method for \code{negbin} objects
}
\value{
The value of \code{val}, as built in the body.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
logLik.negbin(D)
}
