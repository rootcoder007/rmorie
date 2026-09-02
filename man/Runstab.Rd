% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Runstab}
\alias{Runstab}
\title{Table D: exact runs distribution -- Theorem 3.2.2, p. 79}
\usage{
Runstab(n1, n2, r = NULL) 
}
\description{
Table D: exact runs distribution -- Theorem 3.2.2, p. 79
}
\arguments{
\item{n1}{Argument \code{n1}; see Usage.}
\item{n2}{Argument \code{n2}; see Usage.}
\item{r}{Argument \code{r}; see Usage.}
}
\value{
A list with \code{support}, \code{pmf}, \code{cdf}, \code{pmf_r}, \code{cdf_r}, \code{sf_r}, \code{mean}, \code{var}, \code{n1}, \code{n2}.
}
\keyword{internal}
\examples{
rmorie:::Runstab(n1 = 5L, n2 = 5L)
}
