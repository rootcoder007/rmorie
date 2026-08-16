% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.jsonlt_df}
\alias{.jsonlt_df}
\title{As.data.frame() flattens a nested data.frame column into outer.inner}
\usage{
.jsonlt_df(cols, nms)
}
\arguments{
\item{cols}{A vector; its length is taken and its elements indexed.}
\item{nms}{See Usage.}
}
\description{
columns. That is morie_jsonlt_flatten\\'s job, opt-in, and doing it here would make a nested structure impossible to represent at all.
}
\value{
The value of \code{structure}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.jsonlt_df(V, V)
}
