% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.jsonlt_na_token}
\alias{.jsonlt_na_token}
\title{.jsonlt_na_token}
\usage{
.jsonlt_na_token(o)
}
\arguments{
\item{o}{A list; the body reads \code{$na} from it.}
}
\description{
A step of the jsonlt_native implementation. Called by \code{.jsonlt_atomic}, \code{.jsonlt_cell}, \code{.jsonlt_encode}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
.jsonlt_na_token(D)
}
