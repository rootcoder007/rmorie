% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.jsonlt_tidy}
\alias{.jsonlt_tidy}
\title{.jsonlt_tidy}
\usage{
.jsonlt_tidy(s)
}
\arguments{
\item{s}{Compared against \code{"-0"}.}
}
\description{
A step of the jsonlt_native implementation. Called by \code{.jsonlt_num}. See the file header for the source the module follows.
}
\value{
The value of \code{s}, as built in the body.
}
\keyword{internal}
\examples{
.jsonlt_tidy(s = 5L)
}
