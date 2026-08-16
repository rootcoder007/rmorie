% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{endemic_equilibrium}
\alias{endemic_equilibrium}
\title{endemic_equilibrium}
\usage{
endemic_equilibrium(beta, nu, mu, strain = 0L)
}
\arguments{
\item{beta}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{nu}{A vector; its length is taken.}
\item{mu}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{strain}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{0L}.}
}
\description{
A step of the hiatus_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{R0}, \code{S}, \code{I}.
}
\keyword{internal}
\examples{
endemic_equilibrium(beta = 0.5, nu = 0.5, mu = 5L)
}
