% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Wrsci}
\alias{Wrsci}
\title{Rank-sum shift CI -- Sec. 8.2, p. 292}
\usage{
Wrsci(x, y, wcrit) 
}
\description{
Rank-sum shift CI -- Sec. 8.2, p. 292
}
\arguments{
\item{x}{Argument \code{x}; see Usage.}
\item{y}{Argument \code{y}; see Usage.}
\item{wcrit}{Argument \code{wcrit}; see Usage.}
}
\value{
A list with \code{lower}, \code{upper}, \code{k}, \code{estimate}, \code{ndiff}, \code{m}, \code{n}.
}
\keyword{internal}
\examples{
rmorie:::Wrsci(x = 5L, y = c(1, 2, 3, 4, 5, 6, 7, 8), wcrit = 5L)
}
