% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Randtest}
\alias{Randtest}
\title{Randomized decision rule of exact size -- Sec. 1.2.9, pp. 26-27}
\usage{
Randtest(pmf, alpha = 0.05, pmf_alt = NULL) 
}
\description{
Randomized decision rule of exact size -- Sec. 1.2.9, pp. 26-27
}
\arguments{
\item{pmf}{Argument \code{pmf}; see Usage.}
\item{alpha}{Argument \code{alpha}; see Usage.}
\item{pmf_alt}{Argument \code{pmf_alt}; see Usage.}
}
\value{
A list with \code{gamma}, \code{t2}, \code{t1}, \code{size_hard}, \code{size}, \code{power}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
rmorie:::Randtest(V)
}
