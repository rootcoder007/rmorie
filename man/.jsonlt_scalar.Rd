% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.jsonlt_scalar}
\alias{.jsonlt_scalar}
\title{One element of an atomic vector, already known to be non-NA}
\usage{
.jsonlt_scalar(v, o)
}
\arguments{
\item{v}{A flag; the body branches on it.}
\item{o}{A list; the body reads \code{$Date}, \code{$digits} from it.}
}
\description{
A step of the jsonlt_native implementation. Called by \code{.jsonlt_atomic}, \code{.jsonlt_cell}, \code{.jsonlt_encode}. See the file header for the source the module follows.
}
\value{
The value of \code{.jsonlt_esc}.
}
\keyword{internal}
\examples{
.jsonlt_scalar(v = 5L, o = c(1, 2, 3, 4, 5, 6, 7, 8))
}
