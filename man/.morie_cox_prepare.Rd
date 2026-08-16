% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_cox_prepare}
\alias{.morie_cox_prepare}
\title{.morie_cox_prepare}
\usage{
.morie_cox_prepare(time, event, X = NULL)
}
\arguments{
\item{time}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{event}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{X}{Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.}
}
\description{
A step of the cox_native implementation. Called by \code{.morie_aft_common}, \code{morie_breslow_tie_correction}, \code{morie_cause_specific_hazard} and 8 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{t}, \code{e}, \code{X}.
}
\keyword{internal}
