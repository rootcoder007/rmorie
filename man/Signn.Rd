% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Signn}
\alias{Signn}
\title{Sign-test sample size -- eq. (5.4.9), p. 179}
\usage{
Signn(theta, alpha = 0.05, beta = 0.1) 
}
\description{
Sign-test sample size -- eq. (5.4.9), p. 179
}
\arguments{
\item{theta}{Argument \code{theta}; see Usage.}
\item{alpha}{Argument \code{alpha}; see Usage.}
\item{beta}{Argument \code{beta}; see Usage.}
}
\value{
A list with \code{n}, \code{n_raw}, \code{root_n}, \code{z_alpha}, \code{z_beta}, \code{theta}.
}
\keyword{internal}
\examples{
rmorie:::Signn(theta = 0.05)
}
