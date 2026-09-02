% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Wsrci}
\alias{Wsrci}
\title{Walsh-average confidence interval -- Sec. 5.7.5, pp. 207-209}
\usage{
Wsrci(x, tcrit) 
}
\description{
Walsh-average confidence interval -- Sec. 5.7.5, pp. 207-209
}
\arguments{
\item{x}{Argument \code{x}; see Usage.}
\item{tcrit}{Argument \code{tcrit}; see Usage.}
}
\value{
A list with \code{lower}, \code{upper}, \code{coverage}, \code{tail}, \code{nwalsh}, \code{n}, \code{tcrit}, \code{estimate}.
}
\keyword{internal}
\examples{
Wsrci(x = c(1, 2, 3, 4, 5, 6, 7, 8), tcrit = 5L)
}
