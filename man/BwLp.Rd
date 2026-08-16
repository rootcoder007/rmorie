% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{BwLp}
\alias{BwLp}
\title{The book\'s route end to end: prewarp by eq (3.141), place poles by}
\usage{
BwLp(cutoff_hz, order = 4, fs = 1000, z = NULL)
}
\arguments{
\item{cutoff_hz}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{order}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{4}.}
\item{fs}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1000}.}
\item{z}{Defaults to \code{NULL}.}
}
\description{
eq (3.137), keep the left-half-plane ones by eq (3.138), apply the bilinear transform of eq (3.139) to reach eq (3.143).  Prewarping is done here; without it the realized cutoff sits below the request.
}
\value{
The value of \code{r}, as built in the body.
}
\keyword{internal}
\examples{
BwLp(cutoff_hz = 5L)
}
