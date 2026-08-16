% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.attrInf_eq}
\alias{.attrInf_eq}
\title{Python-style == for the accuracy block: bool <-> int, int <-> float}
\usage{
.attrInf_eq(a, b)
}
\arguments{
\item{a}{See Usage.}
\item{b}{See Usage.}
}
\description{
all match (R\\'s == coerces); int vs str does not (mirrors Python: 1 == "1" is False, and isTRUE(1 == "1") is FALSE in R).
}
\value{
A logical value.
}
\keyword{internal}
