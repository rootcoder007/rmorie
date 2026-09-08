% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Degencdf}
\alias{Degencdf}
\title{Degenerate limiting distribution of the sample mean (Deshmukh eq. 10.3)}
\usage{
Degencdf(x, mu) 
}
\description{
Degenerate limiting distribution of the sample mean (Deshmukh eq. 10.3)
}
\arguments{
\item{x}{Argument \code{x}; see Usage.}
\item{mu}{Argument \code{mu}; see Usage.}
}
\value{
A list with \code{cdf}, \code{mu}, \code{x}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
rmorie:::Degencdf(V, V)
}
