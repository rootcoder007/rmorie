% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Mlogitlp}
\alias{Mlogitlp}
\title{Contextual multilevel logit predictor (Lawson eq. 6.8)}
\usage{
Mlogitlp(f, g, R) 
}
\description{
Contextual multilevel logit predictor (Lawson eq. 6.8)
}
\arguments{
\item{f}{Argument \code{f}; see Usage.}
\item{g}{Argument \code{g}; see Usage.}
\item{R}{Argument \code{R}; see Usage.}
}
\value{
A list with \code{eta}, \code{p}, \code{n}.
}
\keyword{internal}
\examples{
rmorie:::Mlogitlp(f = c(1, 2, 3, 4, 5, 6, 7, 8), g = c(1, 2, 3, 4, 5, 6, 7, 8), R = c(1, 2, 3, 4, 5, 6, 7, 8))
}
