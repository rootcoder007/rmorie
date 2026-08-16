% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_nypd_load_sample}
\alias{.morie_nypd_load_sample}
\title{.morie_nypd_load_sample}
\usage{
.morie_nypd_load_sample(which = c("arrests", "complaint"))
}
\arguments{
\item{which}{Defaults to \code{c("arrests", "complaint")}.}
}
\description{
A step of the nypd_analyze implementation. Called by \code{morie_nypd_all_analyses}. See the file header for the source the module follows.
}
\value{
The value of \code{utils::read.csv}.
}
\keyword{internal}
