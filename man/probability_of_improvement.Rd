% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{probability_of_improvement}
\alias{probability_of_improvement}
\title{probability_of_improvement}
\usage{
probability_of_improvement(mu, sd, best, xi = 0) 
}
\arguments{
\item{mu}{Numeric; combined arithmetically in the body.}
\item{sd}{Numeric; combined arithmetically in the body.}
\item{best}{Numeric; combined arithmetically in the body.}
\item{xi}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
}
\description{
A step of the bayopt_native implementation. Called by \code{acquire}. See the file header for the source the module follows.
}
\value{
The value of \code{.Phi}.
}
\keyword{internal}
\examples{
probability_of_improvement(mu = c(1, 2, 3, 4, 5, 6, 7, 8), sd = 5L, best = c(1, 2, 3, 4, 5, 6, 7, 8))
}
