% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bd_parse}
\alias{.bd_parse}
\title{Internal graph helpers for the back-door criterion}
\usage{
.bd_parse(dag)
}
\arguments{
\item{dag}{A matrix; passed to \code{as.matrix}.}
}
\description{
A step of the causal_native implementation. Called by \code{morie_backdoor_criterion}. See the file header for the source the module follows.
}
\value{
A list with \code{children}, \code{parents}, \code{nodes}.
}
\keyword{internal}
