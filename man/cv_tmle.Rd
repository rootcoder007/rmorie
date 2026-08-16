% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{cv_tmle}
\alias{cv_tmle}
\title{cv_tmle}
\usage{
cv_tmle(fold_estimates, fold_ics, n)
}
\arguments{
\item{fold_estimates}{See Usage.}
\item{fold_ics}{See Usage.}
\item{n}{See Usage.}
}
\description{
A step of the tldapar_native implementation. Called by \code{morie_tldapar}. See the file header for the source the module follows.
}
\value{
A list with \code{psi}, \code{se}, \code{ci}, \code{mean_ic}, \code{note}.
}
\keyword{internal}
