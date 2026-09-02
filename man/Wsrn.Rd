% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Wsrn}
\alias{Wsrn}
\title{Signed-rank sample size -- eq. (5.7.15), p. 206}
\usage{
Wsrn(p2, alpha = 0.05, beta = 0.05, twosided = FALSE) 
}
\description{
Signed-rank sample size -- eq. (5.7.15), p. 206
}
\arguments{
\item{p2}{Argument \code{p2}; see Usage.}
\item{alpha}{Argument \code{alpha}; see Usage.}
\item{beta}{Argument \code{beta}; see Usage.}
\item{twosided}{Argument \code{twosided}; see Usage.}
}
\value{
A list with \code{n}, \code{n_raw}, \code{z_alpha}, \code{z_beta}, \code{p2}.
}
\keyword{internal}
\examples{
rmorie:::Wsrn(p2 = 0.7, alpha = 0.05, beta = 0.1)
}
