% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_elmo}
\alias{morie_elmo}
\title{Morie entry point: matches the Python payload keys}
\usage{
morie_elmo(X, layers, raw_weights = NULL, gamma = 1) 
}
\arguments{
\item{X}{Passed to \code{elmo_representation}.}
\item{layers}{Passed to \code{elmo_representation}.}
\item{raw_weights}{Passed to \code{elmo_representation}.}
\item{gamma}{Passed to \code{elmo_representation}. Defaults to \code{1}.}
}
\description{
A step of the elmo_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{elmo_representation}.
}
\keyword{internal}
\examples{
set.seed(2)
d <- 2
X <- matrix(rnorm(6, 0, 0.5), 3, d)
mkw <- function() matrix(rnorm(d * 4 * d, 0, 0.3), d, 4 * d)
layer <- list(Wxf = mkw(), Whf = mkw(), bf = rep(0, 4 * d),
              Wxb = mkw(), Whb = mkw(), bb = rep(0, 4 * d))
r <- morie_elmo(X, list(layer))
str(r, max.level = 1)
}
