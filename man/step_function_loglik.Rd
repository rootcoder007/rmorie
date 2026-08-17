% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{step_function_loglik}
\alias{step_function_loglik}
\title{step_function_loglik}
\usage{
step_function_loglik(y, s, h, L)
}
\arguments{
\item{y}{See Usage.}
\item{s}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{h}{A vector; its length is taken and its elements indexed.}
\item{L}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the bayrjmcmc_native implementation. Called by \code{changepoint_rjmcmc}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
y <- c(0.1, 0.15, 0.2, 0.7, 0.75, 0.85)
step_function_loglik(y, s = 0.5, h = c(3, 4), L = 1)
}
