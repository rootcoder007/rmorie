% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_nypd_result}
\alias{.morie_nypd_result}
\title{.morie_nypd_result}
\usage{
.morie_nypd_result(title, summary_lines = list(), tables = list(), interpretation = "", warnings = "", payload = list())
}
\arguments{
\item{title}{Carried through into a list the body builds.}
\item{summary_lines}{Carried through into a list the body builds. Defaults to \code{list()}.}
\item{tables}{Carried through into a list the body builds. Defaults to \code{list()}.}
\item{interpretation}{Carried through into a list the body builds. Defaults to \code{""}.}
\item{warnings}{Carried through into a list the body builds. Defaults to \code{""}.}
\item{payload}{Carried through into a list the body builds. Defaults to \code{list()}.}
}
\description{
A step of the nypd_analyze implementation. Called by \code{morie_nypd_all_analyses}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_nypd_result(V)
}
