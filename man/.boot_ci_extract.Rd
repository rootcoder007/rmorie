% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.boot_ci_extract}
\alias{.boot_ci_extract}
\title{Internal helper: Boot Ci Extract}
\usage{
.boot_ci_extract(bci, ci_method)
}
\description{
Internal helper: Boot Ci Extract
}
\value{
A vector.
}
\keyword{internal}
\examples{
.boot_ci_extract(bci = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)), ci_method = 5L)
}
