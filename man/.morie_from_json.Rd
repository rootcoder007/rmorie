% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_from_json}
\alias{.morie_from_json}
\title{Internal shim: prefer jsonlite, fall back to the native parser}
\usage{
.morie_from_json(txt, ...)
}
\description{
Internal shim: prefer jsonlite, fall back to the native parser
}
\value{
The value of \code{morie_fetch_json}.
}
\keyword{internal}
\examples{
.morie_from_json(txt = 5L)
}
