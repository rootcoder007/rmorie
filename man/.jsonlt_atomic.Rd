% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.jsonlt_atomic}
\alias{.jsonlt_atomic}
\title{.jsonlt_atomic}
\usage{
.jsonlt_atomic(x, o, unbox_ok)
}
\arguments{
\item{x}{A vector; its length is taken and its elements indexed.}
\item{o}{A list; the body reads \code{$auto_unbox}, \code{$complex}, \code{$digits} from it.}
\item{unbox_ok}{Passed to \code{&&}.}
}
\description{
A step of the jsonlt_native implementation. Called by \code{.jsonlt_cell}, \code{.jsonlt_encode}, \code{.jsonlt_ser}. See the file header for the source the module follows.
}
\value{
A character value.
}
\keyword{internal}
\examples{
.jsonlt_atomic(x = c(1, 2, 3, 4, 5, 6, 7, 8), o = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)), unbox_ok = c(1, 2, 3, 4, 5, 6, 7, 8))
}
