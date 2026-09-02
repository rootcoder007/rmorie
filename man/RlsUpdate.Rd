% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{RlsUpdate}
\alias{RlsUpdate}
\title{Eq. (3.224): w(n) = w(n-1) + k(n) alpha(n)}
\usage{
RlsUpdate(w_prev, k, alpha) 
}
\arguments{
\item{w_prev}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{k}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{alpha}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the rangayyan_adapt implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{w_next}, \code{correction}, \code{alpha}, \code{order}, \code{sign}, \code{erratum}, \code{method}.
}
\keyword{internal}
\examples{
RlsUpdate(w_prev = 5L, k = 5L, alpha = 0.5)
}
