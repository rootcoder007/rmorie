% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Limsupio}
\alias{Limsupio}
\title{Limit-superior event, infinitely often (Deshmukh eq. 6.1)}
\usage{
Limsupio(dev, k) 
}
\description{
Limit-superior event, infinitely often (Deshmukh eq. 6.1)
}
\arguments{
\item{dev}{Argument \code{dev}; see Usage.}
\item{k}{Argument \code{k}; see Usage.}
}
\value{
A list with \code{threshold}, \code{in_event}, \code{prob}, \code{n_paths}.
}
\keyword{internal}
\examples{
Limsupio(dev = c(1, 2, 3, 4, 5, 6, 7, 8), k = 5L)
}
