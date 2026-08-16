% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_dml_guard_end}
\alias{.morie_dml_guard_end}
\title{.morie_dml_guard_end}
\usage{
.morie_dml_guard_end(st)
}
\arguments{
\item{st}{State list returned by `.morie_dml_guard_begin`.}
}
\description{
.morie_dml_guard_end
}
\value{
The value of \code{invisible}.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
.morie_dml_guard_end(D)
}
