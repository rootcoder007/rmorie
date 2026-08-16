% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_cpd_load_sample}
\alias{.morie_cpd_load_sample}
\title{Load a bundled CPD fixture (offline). `which` is "crime" or "arrests"}
\usage{
.morie_cpd_load_sample(which = c("crime", "arrests"))
}
\arguments{
\item{which}{Passed to \code{match.arg}. Defaults to \code{c("crime", "arrests")}.}
}
\description{
A step of the cpd_analyze implementation. Called by \code{morie_cpd_all_analyses}. See the file header for the source the module follows.
}
\value{
The value of \code{utils::read.csv}.
}
\keyword{internal}
\examples{
.morie_cpd_load_sample()
}
