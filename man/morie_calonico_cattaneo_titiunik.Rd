% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_calonico_cattaneo_titiunik}
\alias{morie_calonico_cattaneo_titiunik}
\title{Alias kept from the generated stub\'s signature}
\usage{
morie_calonico_cattaneo_titiunik(y, x, cutoff = 0, ...) 
}
\arguments{
\item{y}{Passed to \code{morie_rdrobu}.}
\item{x}{Passed to \code{morie_rdrobu}.}
\item{cutoff}{Passed to \code{morie_rdrobu}. Defaults to \code{0}.}
\item{...}{Passed through.}
}
\description{
A step of the rdrobu_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{morie_rdrobu}.
}
\keyword{internal}
\examples{
set.seed(1)
x <- runif(200, -1, 1)
y <- 0.5 * x + (x >= 0) * 0.3 + rnorm(200) * 0.1
morie_calonico_cattaneo_titiunik(y, x)
}
