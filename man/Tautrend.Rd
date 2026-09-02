% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Tautrend}
\alias{Tautrend}
\title{Kendall tau trend test -- Sec. 11.2.5, p. 406}
\usage{
Tautrend(y, alternative = "two-sided") 
}
\description{
Kendall tau trend test -- Sec. 11.2.5, p. 406
}
\arguments{
\item{y}{Argument \code{y}; see Usage.}
\item{alternative}{Argument \code{alternative}; see Usage.}
}
\value{
A list with \code{tau}, \code{statistic}, \code{P}, \code{Q}, \code{z}, \code{p_value}, \code{var}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Tautrend(V)
}
