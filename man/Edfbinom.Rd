% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Edfbinom}
\alias{Edfbinom}
\title{EDF count is binomial -- Gibbons Theorem 2.3.1 (book p. 33)}
\usage{
Edfbinom(n, fx, i = NULL) 
}
\description{
EDF count is binomial -- Gibbons Theorem 2.3.1 (book p. 33)
}
\arguments{
\item{n}{Argument \code{n}; see Usage.}
\item{fx}{Argument \code{fx}; see Usage.}
\item{i}{Argument \code{i}; see Usage.}
}
\value{
A list with \code{mean}, \code{var}, \code{pmf}, \code{cdf}, \code{n}, \code{fx}.
}
\keyword{internal}
\examples{
Edfbinom(n = 10, fx = 0.5)
}
