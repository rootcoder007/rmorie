% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_exp3}
\alias{morie_exp3}
\title{morie_exp3}
\usage{
morie_exp3(x, gamma_, T = NULL, seed = 0) 
}
\arguments{
\item{x}{A matrix; indexed by row and column.}
\item{gamma_}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{T}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{seed}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
}
\description{
A step of the exp3_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{actions}, \code{rewards}, \code{probs}, \code{weights}, \code{total_reward}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(1)
r <- morie_exp3(x = rnorm(10), gamma_ = 0.5); TRUE
}
