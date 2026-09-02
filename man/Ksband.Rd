% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Ksband}
\alias{Ksband}
\title{KS confidence band -- Sec. 4.4.2, p. 121}
\usage{
Ksband(x, dcrit, at = NULL) 
}
\description{
KS confidence band -- Sec. 4.4.2, p. 121
}
\arguments{
\item{x}{Argument \code{x}; see Usage.}
\item{dcrit}{Argument \code{dcrit}; see Usage.}
\item{at}{Argument \code{at}; see Usage.}
}
\value{
A list with \code{at}, \code{edf}, \code{lower}, \code{upper}, \code{width}, \code{dcrit}, \code{n}.
}
\keyword{internal}
\examples{
set.seed(2)
Ksband(rnorm(25), dcrit = 0.264)
}
