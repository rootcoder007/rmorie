% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s03json_pretty}
\alias{.s03json_pretty}
\title{.s03json_pretty}
\usage{
.s03json_pretty(txt, indent = 2L)
}
\arguments{
\item{txt}{Passed to \code{morie_jsonlt_prettify}.}
\item{indent}{Passed to \code{morie_jsonlt_prettify}. Defaults to \code{2L}.}
}
\description{
A step of the helpers_s03 implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{morie_jsonlt_prettify}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.s03json_pretty(V)
}
