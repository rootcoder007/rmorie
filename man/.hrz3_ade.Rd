% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.hrz3_ade}
\alias{.hrz3_ade}
\title{Density-weighted average derivative, Horowitz Sec. 2.6.1:}
\usage{
.hrz3_ade(X, y, h)
}
\arguments{
\item{X}{See Usage.}
\item{y}{See Usage.}
\item{h}{See Usage.}
}
\description{
delta = E[f_X(X) dE(Y|X)/dX] = -2 E[f_X\\\'(X) Y]. In a single-index model delta is proportional to beta, so it fixes the index DIRECTION without optimising over it.  The leave-one-out form is used: the own-observation term of a kernel density derivative is identically zero only in the limit, and keeping it biases delta toward zero.
}
\value{
The value of \code{delta}, as built in the body.
}
\keyword{internal}
