% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tree_predict}
\alias{.tree_predict}
\title{Internal helper: predict from one grown tree}
\usage{
.tree_predict(node, X)
}
\description{
Accepts either the flat node table produced by the compiled kernel or the nested list produced by the pure-R reference builder.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
