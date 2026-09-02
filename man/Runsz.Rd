% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Runsz}
\alias{Runsz}
\title{Total runs asymptotic normality -- eq. (3.2.9), p. 82}
\usage{
Runsz(r, n1, n2, correct = FALSE) 
}
\description{
Total runs asymptotic normality -- eq. (3.2.9), p. 82
}
\arguments{
\item{r}{Argument \code{r}; see Usage.}
\item{n1}{Argument \code{n1}; see Usage.}
\item{n2}{Argument \code{n2}; see Usage.}
\item{correct}{Argument \code{correct}; see Usage.}
}
\value{
A list with \code{z}, \code{z_exact}, \code{p_value}, \code{mean}, \code{var}, \code{mean_exact}, \code{var_exact}, \code{lam}, \code{n}.
}
\keyword{internal}
\examples{
rmorie:::Runsz(r = 9, n1 = 10, n2 = 10)
}
