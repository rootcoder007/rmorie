% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Ctrlmedcur}
\alias{Ctrlmedcur}
\title{Curtailed control median test -- eq. (6.5.2), p. 258}
\usage{
Ctrlmedcur(m, n, alpha = 0.05) 
}
\description{
Curtailed control median test -- eq. (6.5.2), p. 258
}
\arguments{
\item{m}{Argument \code{m}; see Usage.}
\item{n}{Argument \code{n}; see Usage.}
\item{alpha}{Argument \code{alpha}; see Usage.}
}
\value{
A list with \code{d}, \code{d_raw}, \code{d_exact}, \code{alpha_exact}, \code{z_alpha}, \code{m}, \code{n}.
}
\keyword{internal}
\examples{
Ctrlmedcur(m = 5L, n = 5L)
}
