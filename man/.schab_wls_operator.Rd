% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_wls_operator}
\alias{.schab_wls_operator}
\title{The cutoff is numpy.linalg.pinv\\\'s default: max(dim) * eps * largest}
\usage{
.schab_wls_operator(X, w)
}
\arguments{
\item{X}{See Usage.}
\item{w}{See Usage.}
}
\description{
singular value. Written out rather than taken from MASS::ginv because MASS is only in Suggests.
}
\value{
The value of \code{op}, as built in the body.
}
\keyword{internal}
