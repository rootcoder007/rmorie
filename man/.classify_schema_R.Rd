% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.classify_schema_R}
\alias{.classify_schema_R}
\title{Internal helper: Classify Schema R}
\usage{
.classify_schema_R(schema, dataset_name)
}
\description{
Internal helper: Classify Schema R
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
.classify_schema_R(D, V)
}
