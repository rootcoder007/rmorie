% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.jsonlt_posix}
\alias{.jsonlt_posix}
\title{.jsonlt_posix}
\usage{
.jsonlt_posix(v, o)
}
\arguments{
\item{v}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{o}{A list; the body reads \code{$digits}, \code{$POSIXt} from it.}
}
\description{
A step of the jsonlt_native implementation. Called by \code{.jsonlt_scalar}. See the file header for the source the module follows.
}
\value{
The value of \code{.jsonlt_esc}.
}
\keyword{internal}
