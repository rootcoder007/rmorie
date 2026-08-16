% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bd_desc}
\alias{.bd_desc}
\title{.bd_desc}
\usage{
.bd_desc(node, ch)
}
\arguments{
\item{node}{See Usage.}
\item{ch}{A vector; indexed elementwise.}
}
\description{
A step of the causal_native implementation. Called by \code{.bd_blocked}, \code{morie_backdoor_criterion}. See the file header for the source the module follows.
}
\value{
The value of \code{seen}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.bd_desc(V, V)
}
