% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{print.morie_rdd}
\alias{print.morie_rdd}
\title{Print method for \code{morie_rdd} objects}
\usage{
\method{print}{morie_rdd}(x, ...) 
}
\arguments{
\item{x}{A \code{morie_rdd} object.}
\item{...}{Ignored; accepted for S3 consistency.}
}
\description{
Print method for \code{morie_rdd} objects
}
\value{
The value of \code{invisible}.
}
\keyword{internal}
\examples{
set.seed(1)
n <- 500
xr <- runif(n, -1, 1)
tr <- as.integer(xr >= 0)
yy <- 0.5 * xr + 0.4 * tr + rnorm(n) * 0.3
df <- data.frame(y = yy, x = xr)
fit <- morie_rdd(df, "y", "x", cutoff = 0)
print(fit)
}
