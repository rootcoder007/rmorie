% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.barerp_num_grad}
\alias{.barerp_num_grad}
\title{.barerp_num_grad}
\usage{
.barerp_num_grad(f, x, h = 1e-06)
}
\arguments{
\item{f}{See Usage.}
\item{x}{A vector; its length is taken and its elements indexed.}
\item{h}{Numeric; combined arithmetically in the body. Defaults to \code{1e-06}.}
}
\description{
A step of the barerp_native implementation. Called by \code{grad.Fun}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
