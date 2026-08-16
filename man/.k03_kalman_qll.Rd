% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.k03_kalman_qll}
\alias{.k03_kalman_qll}
\title{# Gaussian quasi log-likelihood from the Kalman filter}
\usage{
.k03_kalman_qll(y, mu, phi, sig2)
}
\arguments{
\item{y}{A vector; its length is taken and its elements indexed.}
\item{mu}{Numeric; combined arithmetically in the body.}
\item{phi}{Numeric; combined arithmetically in the body.}
\item{sig2}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the k03volsv implementation. Called by \code{Volsv}. See the file header for the source the module follows.
}
\value{
The value of \code{ll}, as built in the body.
}
\keyword{internal}
