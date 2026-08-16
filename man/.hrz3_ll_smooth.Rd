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
\item{z}{See Usage.}
\item{y}{See Usage.}
\item{zq}{See Usage.}
\item{h}{See Usage.}
}
\description{
Local linear reproduces an affine function exactly, which is what makes an exact-recovery anchor possible for the additive fits built on it; Nadaraya-Watson does not.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
