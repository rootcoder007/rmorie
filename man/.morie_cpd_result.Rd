% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_cpd_result}
\alias{.morie_cpd_result}
\title{Build a rich-result-shaped surface payload}
\usage{
.morie_cpd_result(title, summary_lines = list(), tables = list(), interpretation = "", warnings = "", payload = list())
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
A step of the cpd_analyze implementation. Called by \code{morie_cpd_all_analyses}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
