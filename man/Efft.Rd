% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Efft}
\alias{Efft}
\title{One-sample t efficacy -- eq. (13.3.2), p. 488}
\usage{
Efft(n, sigma2) 
}
\description{
One-sample t efficacy -- eq. (13.3.2), p. 488
}
\arguments{
\item{n}{Argument \code{n}; see Usage.}
\item{sigma2}{Argument \code{sigma2}; see Usage.}
}
\value{
A list with \code{efficacy}, \code{per_obs}, \code{n}, \code{sigma2}.
}
\keyword{internal}
\examples{
rmorie:::Efft(n = 5L, sigma2 = 5L)
}
