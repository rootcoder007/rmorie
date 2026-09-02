% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Mwun}
\alias{Mwun}
\title{Mann-Whitney sample size -- eq. (6.6.18), p. 269 (Noether 1987)}
\usage{
Mwun(p, c = 0.5, alpha = 0.05, beta = 0.1, twosided = FALSE) 
}
\description{
Mann-Whitney sample size -- eq. (6.6.18), p. 269 (Noether 1987)
}
\arguments{
\item{p}{Argument \code{p}; see Usage.}
\item{c}{Argument \code{c}; see Usage.}
\item{alpha}{Argument \code{alpha}; see Usage.}
\item{beta}{Argument \code{beta}; see Usage.}
\item{twosided}{Argument \code{twosided}; see Usage.}
}
\value{
A list with \code{n}, \code{n_raw}, \code{m}, \code{n_y}, \code{z_alpha}, \code{z_beta}, \code{c}, \code{p}.
}
\keyword{internal}
\examples{
Mwun(p = 0.05)
}
