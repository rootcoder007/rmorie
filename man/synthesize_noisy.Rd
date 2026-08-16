% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{synthesize_noisy}
\alias{synthesize_noisy}
\title{synthesize_noisy}
\usage{
synthesize_noisy(X, fraction = 0.1, feature_values = NULL, seed = 0)
}
\arguments{
\item{X}{A vector; indexed elementwise.}
\item{fraction}{Defaults to \code{0.1}.}
\item{feature_values}{Defaults to \code{NULL}.}
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
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
synthesize_noisy(V)
}
