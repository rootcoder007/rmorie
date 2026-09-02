% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Signpow}
\alias{Signpow}
\title{Power of the sign test -- eq. (5.4.8), Table 5.4.1, p. 174}
\usage{
Signpow(n, theta, alpha = 0.05, exact = TRUE) 
}
\description{
Power of the sign test -- eq. (5.4.8), Table 5.4.1, p. 174
}
\arguments{
\item{n}{Argument \code{n}; see Usage.}
\item{theta}{Argument \code{theta}; see Usage.}
\item{alpha}{Argument \code{alpha}; see Usage.}
\item{exact}{Argument \code{exact}; see Usage.}
}
\value{
A list with \code{power}, \code{power_exact}, \code{k_alpha}, \code{alpha_exact}, \code{n}, \code{theta}.
}
\keyword{internal}
\examples{
Signpow(n = 5L, theta = 0.5)
}
