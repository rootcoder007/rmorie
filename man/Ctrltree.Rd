% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Ctrltree}
\alias{Ctrltree}
\title{Treatments-vs-control precedence test -- eq. (10.7.3), p. 373}
\usage{
Ctrltree(samples, r = NULL) 
}
\description{
Treatments-vs-control precedence test -- eq. (10.7.3), p. 373
}
\arguments{
\item{samples}{Argument \code{samples}; see Usage.}
\item{r}{Argument \code{r}; see Usage.}
}
\value{
A list with \code{statistic}, \code{p_value}, \code{pmf}, \code{t}, \code{r}, \code{mtreat}, \code{mean}, \code{k}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Ctrltree(V)
}
