% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Chik2}
\alias{Chik2}
\title{k x 2 equal-proportions test -- eq. (14.3.2), p. 514}
\usage{
Chik2(successes, ns) 
}
\description{
k x 2 equal-proportions test -- eq. (14.3.2), p. 514
}
\arguments{
\item{successes}{Argument \code{successes}; see Usage.}
\item{ns}{Argument \code{ns}; see Usage.}
}
\value{
A list with \code{statistic}, \code{df}, \code{p_value}, \code{phat}, \code{props}, \code{k}, \code{n}.
}
\keyword{internal}
\examples{
rmorie:::Chik2(successes = c(18, 12, 25), ns = c(30, 30, 40))
}
