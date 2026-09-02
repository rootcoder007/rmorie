% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_gwrcal_global_aicc}
\alias{morie_gwrcal_global_aicc}
\title{morie_gwrcal_global_aicc}
\usage{
morie_gwrcal_global_aicc(y, X) 
}
\description{
morie_gwrcal_global_aicc
}
\arguments{
\item{y}{Argument \code{y}; see Usage.}
\item{X}{Argument \code{X}; see Usage.}
}
\value{
The value of \code{.gwr_aicc}.
}
\keyword{internal}
\examples{
set.seed(1)
n <- 50
X <- cbind(1, rnorm(n))
y <- X \%*\% c(1, 2) + rnorm(n)
morie_gwrcal_global_aicc(y, X)
}
