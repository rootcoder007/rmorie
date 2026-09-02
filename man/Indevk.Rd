% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Indevk}
\alias{Indevk}
\title{Independence of k events (Deshmukh eq. 5.1)}
\usage{
Indevk(p, joint) 
}
\description{
Independence of k events (Deshmukh eq. 5.1)
}
\arguments{
\item{p}{Argument \code{p}; see Usage.}
\item{joint}{Argument \code{joint}; see Usage.}
}
\value{
A list with \code{n_conditions}, \code{max_deviation}, \code{independent}, \code{k}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
S <- c("a", "b", "c")
Indevk(S, V)
}
