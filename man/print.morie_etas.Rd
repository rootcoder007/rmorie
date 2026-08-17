% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{print.morie_etas}
\alias{print.morie_etas}
\title{Print method for \code{morie_etas} objects}
\usage{
print.morie_etas(x, ...)
}
\arguments{
\item{x}{A \code{morie_etas} object.}
\item{...}{Ignored; accepted for S3 consistency.}
}
\description{
Print method for \code{morie_etas} objects
}
\value{
The value of \code{invisible}.
}
\keyword{internal}
\examples{
set.seed(1)
times <- sort(runif(80, 0, 200))
fit <- morie_crim_etas(times)
out <- capture.output(print(fit))
any(grepl("ETAS", out))
}
