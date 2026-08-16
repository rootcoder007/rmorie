% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_check_factor}
\alias{.morie_check_factor}
\title{Internal helper: Morie Check Factor}
\usage{
.morie_check_factor(x, ordered = NA, arg = "x")
}
\description{
Internal helper: Morie Check Factor
}
\value{
The value of \code{x}, as built in the body.
}
\keyword{internal}
\examples{
of <- factor(c("lo", "hi", "lo"), levels = c("lo", "hi"), ordered = TRUE)
.morie_check_factor(of, ordered = TRUE)
}
