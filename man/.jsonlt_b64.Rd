% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.jsonlt_b64}
\alias{.jsonlt_b64}
\title{Base64 without a library, so both arms produce the same string}
\usage{
.jsonlt_b64(data)
}
\arguments{
\item{data}{Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the jsonlt_native implementation. Called by \code{.jsonlt_raw_enc}, \code{.jsonlt_ser}. See the file header for the source the module follows.
}
\value{
A character value.
}
\keyword{internal}
