% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.grclus_gains}
\alias{.grclus_gains}
\title{Region growing ----}
\usage{
.grclus_gains(adj, parts)
}
\arguments{
\item{adj}{A vector; its length is taken and its elements indexed.}
\item{parts}{A vector; indexed elementwise.}
}
\description{
A step of the grclus_native implementation. Called by \code{.grclus_balance_bisection}, \code{.grclus_kl}. See the file header for the source the module follows.
}
\value{
The value of \code{g}, as built in the body.
}
\keyword{internal}
