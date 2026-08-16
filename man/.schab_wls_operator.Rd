% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_wls_operator}
\alias{.schab_wls_operator}
\title{The cutoff is numpy.linalg.pinv\\'s default: max(dim) * eps * largest}
\usage{
.schab_wls_operator(X, w)
}
\arguments{
\item{X}{A matrix; passed to \code{nrow}.}
\item{w}{Numeric; passed to \code{sqrt}.}
}
\description{
singular value. Written out rather than taken from MASS::ginv because MASS is only in Suggests.
}
\value{
The value of \code{op}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.schab_wls_operator(V, V)
}
