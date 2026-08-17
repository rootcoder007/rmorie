% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{cfconv}
\alias{cfconv}
\title{cfconv}
\usage{
cfconv(X, R, filter_net, cutoff = 5, ...)
}
\arguments{
\item{X}{Passed to \code{.schn_mat}.}
\item{R}{Passed to \code{.schn_mat}.}
\item{filter_net}{Accepted by the signature and not used anywhere in the body.}
\item{cutoff}{Passed to \code{cosine_cutoff}. Defaults to \code{5}.}
\item{...}{Passed through.}
}
\description{
A step of the schN_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
set.seed(1)
n <- 5; d <- 4
X <- matrix(rnorm(n * d), n, d)
R <- matrix(rnorm(n * 3), n, 3)
filter_net <- function(g) rep(0.1, d)
out <- cfconv(X, R, filter_net, cutoff = 5)
dim(out)
}
