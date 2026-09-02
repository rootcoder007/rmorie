% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Runscrit}
\alias{Runscrit}
\title{Exact runs-test critical region -- Sec. 3.2, p. 84; Table D}
\usage{
Runscrit(n1, n2, alpha = 0.05, tail = "two-sided") 
}
\description{
Exact runs-test critical region -- Sec. 3.2, p. 84; Table D
}
\arguments{
\item{n1}{Argument \code{n1}; see Usage.}
\item{n2}{Argument \code{n2}; see Usage.}
\item{alpha}{Argument \code{alpha}; see Usage.}
\item{tail}{Argument \code{tail}; see Usage.}
}
\value{
A list with \code{lower}, \code{upper}, \code{alpha_lower}, \code{alpha_upper}, \code{alpha_exact}, \code{n1}, \code{n2}.
}
\keyword{internal}
\examples{
rmorie:::Runscrit(n1 = 5L, n2 = 5L)
}
