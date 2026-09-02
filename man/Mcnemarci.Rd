% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Mcnemarci}
\alias{Mcnemarci}
\title{McNemar CI -- Sec. 14.5, eq. (14.5.2), p. 523}
\usage{
Mcnemarci(table, alpha = 0.05) 
}
\description{
McNemar CI -- Sec. 14.5, eq. (14.5.2), p. 523
}
\arguments{
\item{table}{Argument \code{table}; see Usage.}
\item{alpha}{Argument \code{alpha}; see Usage.}
}
\value{
A list with \code{estimate}, \code{lower}, \code{upper}, \code{se}, \code{se_null}, \code{n}.
}
\keyword{internal}
\examples{
rmorie:::Mcnemarci(rbind(c(20, 5), c(12, 13)))
}
