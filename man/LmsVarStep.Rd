% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{LmsVarStep}
\alias{LmsVarStep}
\title{Eq. (3.204): eq. (3.203) with a step size that changes each sample}
\usage{
LmsVarStep(w, e, rvec, mu_n) 
}
\arguments{
\item{w}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{e}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{rvec}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{mu_n}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the rangayyan_adapt implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{w_next}, \code{mu}, \code{e}, \code{order}, \code{time_varying}, \code{method}.
}
\keyword{internal}
\examples{
LmsVarStep(w = c(1, 2, 3, 4, 5, 6, 7, 8), e = c(1, 2, 3, 4, 5, 6, 7, 8),
  rvec = c(1, 2, 3, 4, 5, 6, 7, 8), mu_n = c(1, 2, 3, 4, 5, 6, 7, 8))
}
