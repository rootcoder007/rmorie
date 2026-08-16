% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{DftX}
\alias{DftX}
\title{Eq (3.80) with bin k at k fs / N.  Figure 3.38: for even N, DC and}
\usage{
DftX(x, fs = 1)
}
\arguments{
\item{x}{Passed to \code{Dft}.}
\item{fs}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
}
\description{
the folding frequency fs/2 are the two real-valued bins.
}
\value{
The value of \code{r}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
DftX(V)
}
