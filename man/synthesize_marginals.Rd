% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{synthesize_marginals}
\alias{synthesize_marginals}
\title{synthesize_marginals}
\usage{
synthesize_marginals(X, n, seed = 0) 
}
\arguments{
\item{X}{A vector; its length is taken and its elements indexed.}
\item{n}{A count; the body uses it as \code{seq_len(...)}.}
\item{seed}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
}
\description{
A step of the memb_native implementation. Called by \code{morie_memb}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
synthesize_marginals(X = c(1, 2, 3, 4, 5, 6, 7, 8), n = 5L)
}
