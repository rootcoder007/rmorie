% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_dag_desc}
\alias{.morie_dag_desc}
\title{Internal helper: descendants of a node (including itself)}
\usage{
.morie_dag_desc(g, v)
}
\description{
Internal helper: descendants of a node (including itself)
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
.morie_dag_desc(D, V)
}
