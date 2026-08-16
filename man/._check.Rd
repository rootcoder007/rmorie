% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{._check}
\alias{._check}
\title{._check}
\usage{
._check(beta, nu, mu, sigma)
}
\arguments{
\item{beta}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{nu}{A vector; its length is taken.}
\item{mu}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{sigma}{A matrix; passed to \code{as.matrix}.}
}
\description{
A step of the hiatus_native implementation. Called by \code{.hiatus_simulate}, \code{derivatives}. See the file header for the source the module follows.
}
\value{
A list with \code{b}, \code{nv}, \code{m}, \code{sg}, \code{n}.
}
\keyword{internal}
\examples{
._check(beta = 0.5, nu = 0.5, mu = 5L, sigma = 0.5)
}
