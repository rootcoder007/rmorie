% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Medtestpow}
\alias{Medtestpow}
\title{Precedence/median-test power -- eqs. (6.4.9)-(6.4.10), p. 254}
\usage{
Medtestpow(m, n, r, wcrit, g, nodes = 2001) 
}
\description{
Precedence/median-test power -- eqs. (6.4.9)-(6.4.10), p. 254
}
\arguments{
\item{m}{Argument \code{m}; see Usage.}
\item{n}{Argument \code{n}; see Usage.}
\item{r}{Argument \code{r}; see Usage.}
\item{wcrit}{Argument \code{wcrit}; see Usage.}
\item{g}{Argument \code{g}; see Usage.}
\item{nodes}{Argument \code{nodes}; see Usage.}
}
\value{
A list with \code{power}, \code{pmf}, \code{m}, \code{n}, \code{r}, \code{wcrit}.
}
\keyword{internal}
\examples{
rmorie:::Medtestpow(m = 9, n = 8, r = 5, wcrit = 2, g = function(u) u^2)
}
