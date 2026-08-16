% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Ahi}
\alias{Ahi}
\title{Ahi}
\usage{
Ahi(airflow, fs, spo2 = NULL, hours = NULL, apneafrac = 0.1, hypofrac = 0.5, minsec = 10, desat = 0, envsec = 1)
}
\arguments{
\item{airflow}{See Usage.}
\item{fs}{See Usage.}
\item{spo2}{Defaults to \code{NULL}.}
\item{hours}{Defaults to \code{NULL}.}
\item{apneafrac}{Defaults to \code{0.1}.}
\item{hypofrac}{Defaults to \code{0.5}.}
\item{minsec}{Defaults to \code{10}.}
\item{desat}{Defaults to \code{0}.}
\item{envsec}{Defaults to \code{1}.}
}
\description{
Part of the rangayyan_class2 implementation; see the file header for the source it follows.
}
\value{
A list with \code{ahi}, \code{severity}, \code{apnea}, \code{hypopnea}, \code{events}, \code{hours}, \code{baseline}, \code{envsec}, \code{oxygenchecked}, \code{method}.
}
\keyword{internal}
\examples{
Ahi(airflow = c(1, 2, 3, 4, 5, 6, 7, 8), fs = 5L)
}
