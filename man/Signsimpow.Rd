% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Signsimpow}
\alias{Signsimpow}
\title{Simulated sign-test power over supplied samples -- Sec. 5.4.5, p. 175}
\usage{
Signsimpow(samples, m0, kcrit) 
}
\description{
Simulated sign-test power over supplied samples -- Sec. 5.4.5, p. 175
}
\arguments{
\item{samples}{Argument \code{samples}; see Usage.}
\item{m0}{Argument \code{m0}; see Usage.}
\item{kcrit}{Argument \code{kcrit}; see Usage.}
}
\value{
A list with \code{power}, \code{rejections}, \code{nsim}, \code{kmean}, \code{kcrit}.
}
\keyword{internal}
\examples{
rmorie:::Signsimpow(samples = c(1, 2, 3, 4, 5, 6, 7, 8), m0 = c(1, 2, 3, 4, 5, 6, 7, 8),
  kcrit = c(1, 2, 3, 4, 5, 6, 7, 8))
}
