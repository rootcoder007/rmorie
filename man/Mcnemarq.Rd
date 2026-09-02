% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Mcnemarq}
\alias{Mcnemarq}
\title{McNemar test -- eq. (14.5.1), p. 523}
\usage{
Mcnemarq(table, correct = FALSE) 
}
\description{
McNemar test -- eq. (14.5.1), p. 523
}
\arguments{
\item{table}{Argument \code{table}; see Usage.}
\item{correct}{Argument \code{correct}; see Usage.}
}
\value{
A list with \code{statistic}, \code{df}, \code{p_value}, \code{p_exact}, \code{x12}, \code{x21}, \code{ndisc}.
}
\keyword{internal}
\examples{
Mcnemarq(rbind(c(20, 5), c(12, 13)))
}
