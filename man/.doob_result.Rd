% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.doob_result}
\alias{.doob_result}
\title{Internal helper: Doob Result}
\usage{
.doob_result(title, summary_lines, tables = list(), interpretation = NULL, payload = list(), warnings = character())
}
\description{
Internal helper: Doob Result
}
\value{
The value of \code{structure}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.doob_result(V, V)
}
