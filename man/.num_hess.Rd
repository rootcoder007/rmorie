% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.num_hess}
\alias{.num_hess}
\title{.num_hess}
\usage{
.num_hess(f, x, h = 1e-04)
}
\arguments{
\item{f}{Accepted by the signature and not used anywhere in the body.}
\item{x}{A vector; its length is taken and its elements indexed.}
\item{h}{Numeric; combined arithmetically in the body. Defaults to \code{1e-04}.}
}
\description{
A step of the barerp_native implementation. Called by \code{hess.Fun}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
