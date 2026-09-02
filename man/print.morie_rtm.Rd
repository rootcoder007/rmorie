% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{print.morie_rtm}
\alias{print.morie_rtm}
\title{Print method for \code{morie_rtm} objects}
\usage{
\method{print}{morie_rtm}(x, ...) 
}
\arguments{
\item{x}{A \code{morie_rtm} object.}
\item{...}{Ignored; accepted for S3 consistency.}
}
\description{
Print method for \code{morie_rtm} objects
}
\value{
The value of \code{invisible}.
}
\keyword{internal}
\examples{
set.seed(1)
incidents <- cbind(runif(50), runif(50))
layers <- list(schools = cbind(runif(8), runif(8)),
               bars = cbind(runif(6), runif(6)))
fit <- morie_crim_risk_terrain(incidents, layers, n_grid = 10L)
out <- capture.output(print(fit))
any(grepl("Risk terrain", out))
}
