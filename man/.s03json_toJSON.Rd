% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s03json_toJSON}
\alias{.s03json_toJSON}
\title{.s03json_toJSON}
\usage{
.s03json_toJSON(x, auto_unbox = TRUE, digits = NULL, pretty = FALSE, ...)
}
\arguments{
\item{x}{Passed to \code{morie_jsonlt_to_json}.}
\item{auto_unbox}{Passed to \code{morie_jsonlt_to_json}. Defaults to \code{TRUE}.}
\item{digits}{Passed to \code{morie_jsonlt_to_json}.}
\item{pretty}{Passed to \code{morie_jsonlt_to_json}. Defaults to \code{FALSE}.}
\item{...}{Passed through.}
}
\description{
A step of the helpers_s03 implementation. Called by \code{.morie_to_json}, \code{.s03json_write}, \code{jsonlite_toJSON_or_stub} and 1 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{morie_jsonlt_to_json}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.s03json_toJSON(V)
}
