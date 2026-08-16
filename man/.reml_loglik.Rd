% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.reml_loglik}
\alias{.reml_loglik}
\title{.reml_loglik}
\usage{
.reml_loglik(gs, ns, s2a, s2e)
}
\arguments{
\item{gs}{A vector; its length is taken and its elements indexed.}
\item{ns}{A vector; indexed elementwise.}
\item{s2a}{Numeric; combined arithmetically in the body.}
\item{s2e}{Numeric; passed to \code{log}.}
}
\description{
A step of the ranova_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{ll}, \code{mu}.
}
\keyword{internal}
