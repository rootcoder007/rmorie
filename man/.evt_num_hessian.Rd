% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.evt_num_hessian}
\alias{.evt_num_hessian}
\title{Central-difference observed information (the numeric-differencing}
\usage{
.evt_num_hessian(f, theta, h = 1e-04)
}
\arguments{
\item{f}{See Usage.}
\item{theta}{A vector; its length is taken and its elements indexed.}
\item{h}{Numeric; combined arithmetically in the body. Defaults to \code{1e-04}.}
}
\description{
route Coles p. 56 prescribes)
}
\value{
The value of \code{H}, as built in the body.
}
\keyword{internal}
