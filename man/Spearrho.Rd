% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Spearrho}
\alias{Spearrho}
\title{Spearman rank correlation -- Sec. 11.3, eq. (11.3.2), p. 407}
\usage{
Spearrho(x, y) 
}
\description{
Spearman rank correlation -- Sec. 11.3, eq. (11.3.2), p. 407
}
\arguments{
\item{x}{Argument \code{x}; see Usage.}
\item{y}{Argument \code{y}; see Usage.}
}
\value{
A list with \code{statistic}, \code{r_shortcut}, \code{sumd2}, \code{tied}, \code{var}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Spearrho(V, V)
}
