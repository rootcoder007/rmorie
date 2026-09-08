% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Ahi}
\alias{Ahi}
\title{Ahi}
\usage{
Ahi(airflow, fs, spo2 = NULL, hours = NULL, apneafrac = 0.1, hypofrac = 0.5, 
    minsec = 10, desat = 0, envsec = 1) 
}
\arguments{
\item{airflow}{Passed to \code{.morie_bx_vec}.}
\item{fs}{Numeric; combined arithmetically in the body.}
\item{spo2}{Optional; may be \code{NULL}. Passed to \code{.morie_bx_vec}.}
\item{hours}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{apneafrac}{Numeric; combined arithmetically in the body. Defaults to \code{0.1}.}
\item{hypofrac}{Numeric; combined arithmetically in the body. Defaults to \code{0.5}.}
\item{minsec}{Numeric; combined arithmetically in the body. Defaults to \code{10}.}
\item{desat}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
\item{envsec}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
}
\description{
A step of the rangayyan_class2 implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{ahi}, \code{severity}, \code{apnea}, \code{hypopnea}, \code{events}, \code{hours}, \code{baseline}, \code{envsec}, \code{oxygenchecked}, \code{method}.
}
\keyword{internal}
\examples{
Ahi(airflow = c(1, 2, 3, 4, 5, 6, 7, 8), fs = 5L)
}
