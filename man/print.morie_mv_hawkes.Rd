% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{print.morie_mv_hawkes}
\alias{print.morie_mv_hawkes}
\title{Print method for \code{morie_mv_hawkes} objects}
\usage{
\method{print}{morie_mv_hawkes}(x, ...) 
}
\arguments{
\item{x}{A \code{morie_mv_hawkes} object.}
\item{...}{Ignored; accepted for S3 consistency.}
}
\description{
Print method for \code{morie_mv_hawkes} objects
}
\value{
The value of \code{invisible}.
}
\keyword{internal}
\examples{
set.seed(1)
times <- sort(runif(60, 0, 200))
marks <- sample(c("A", "B"), 60, replace = TRUE)
fit <- morie_crim_hawkes_multivariate(times, marks)
out <- capture.output(print(fit))
any(grepl("Hawkes", out))
}
