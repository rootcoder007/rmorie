% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.abcgp.gp_predict}
\alias{.abcgp.gp_predict}
\title{.abcgp.gp_predict}
\usage{
.abcgp.gp_predict(fit, theta)
}
\arguments{
\item{fit}{A list; the body reads \code{$Ainv_r}, \code{$beta}, \code{$chol}, \code{$design}, \code{$dim}, \code{$H}, \code{$HtAinvH_chol}, \code{$kernel}, \code{$lengthscale}, \code{$n}, \code{$tau2} from it.}
\item{theta}{See Usage.}
}
\description{
A step of the abcgp_native implementation. Called by \code{.abcgp.implausible}, \code{morie_abcgp}. See the file header for the source the module follows.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
