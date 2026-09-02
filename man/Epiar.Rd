% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Epiar}
\alias{Epiar}
\title{Epidemic log-autoregression (Lawson eq. 18.3)}
\usage{
Epiar(beta0, beta1, i_lag, b1) 
}
\description{
Epidemic log-autoregression (Lawson eq. 18.3)
}
\arguments{
\item{beta0}{Argument \code{beta0}; see Usage.}
\item{beta1}{Argument \code{beta1}; see Usage.}
\item{i_lag}{Argument \code{i_lag}; see Usage.}
\item{b1}{Argument \code{b1}; see Usage.}
}
\value{
A list with \code{logf}, \code{f}, \code{n}.
}
\keyword{internal}
\examples{
rmorie:::Epiar(beta0 = c(1, 2, 3, 4, 5, 6, 7, 8), beta1 = c(1, 2, 3, 4, 5, 6, 7, 8), i_lag = c(1, 2, 3, 4, 5, 6, 7, 8), b1 = c(1, 2, 3, 4, 5, 6, 7, 8))
}
