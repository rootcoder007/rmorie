% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_to_json}
\alias{.morie_to_json}
\title{Internal shim: prefer .s03json_toJSON, fall back to native}
\usage{
.morie_to_json(x, ...)
}
\description{
Internal shim: prefer .s03json_toJSON, fall back to native
}
\value{
The value of \code{morie_json_stringify}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_to_json(V)
}
