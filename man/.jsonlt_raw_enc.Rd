% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.jsonlt_raw_enc}
\alias{.jsonlt_raw_enc}
\title{.jsonlt_raw_enc}
\usage{
.jsonlt_raw_enc(rv, o)
}
\arguments{
\item{rv}{Passed to \code{.jsonlt_b64}.}
\item{o}{A list; the body reads \code{$raw} from it.}
}
\description{
A step of the jsonlt_native implementation. Called by \code{.jsonlt_cell}, \code{.jsonlt_encode}. See the file header for the source the module follows.
}
\value{
The value of \code{.jsonlt_esc}.
}
\keyword{internal}
