% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.jsonlt_num}
\alias{.jsonlt_num}
\title{The one place a double becomes text. Both arms go through C\\'s printf}
\usage{
.jsonlt_num(x, digits)
}
\arguments{
\item{x}{See Usage.}
\item{digits}{Optional; may be \code{NULL}. Passed to \code{.jsonlt_sig}.}
}
\description{
with the same format string, so the bytes match; each language\\'s own float-to-string would put them one ulp apart and call it a failure.
}
\value{
The value of \code{.jsonlt_tidy}.
}
\keyword{internal}
\examples{
.jsonlt_num(x = 5L, digits = 5L)
}
