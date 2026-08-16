% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.hrz3_ll_smooth}
\alias{.hrz3_ll_smooth}
\title{Local-linear regression of y on z evaluated at zq (Appendix A.3)}
\usage{
.hrz3_ll_smooth(z, y, zq, h)
}
\arguments{
\item{z}{A vector; its length is taken.}
\item{y}{A vector; its length is taken.}
\item{zq}{See Usage.}
\item{h}{Numeric; combined arithmetically in the body.}
}
\description{
Local linear reproduces an affine function exactly, which is what makes an exact-recovery anchor possible for the additive fits built on it; Nadaraya-Watson does not.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
