% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Kwchi}
\alias{Kwchi}
\title{Chi-square approximation to H -- Sec. 10.4.1, p. 357}
\usage{
Kwchi(h, k, ns = NULL) 
}
\description{
Chi-square approximation to H -- Sec. 10.4.1, p. 357
}
\arguments{
\item{h}{Argument \code{h}; see Usage.}
\item{k}{Argument \code{k}; see Usage.}
\item{ns}{Argument \code{ns}; see Usage.}
}
\value{
A list with \code{statistic}, \code{df}, \code{p_value}, \code{table_k}, \code{k}.
}
\keyword{internal}
\examples{
Kwchi(h = 0.5, k = 5L)
}
