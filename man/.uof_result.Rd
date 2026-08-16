% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.uof_result}
\alias{.uof_result}
\title{Internal helper: Uof Result}
\usage{
.uof_result(title, call, summary_lines = list(), warnings = character(0), interpretation = "", ...)
}
\description{
Internal helper: Uof Result
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.uof_result(V, V)
}
