% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.cv_bandwidth}
\alias{.cv_bandwidth}
\title{.cv_bandwidth}
\usage{
.cv_bandwidth(xv, av, fit, n_folds)
}
\arguments{
\item{xv}{A vector; its length is taken and its elements indexed.}
\item{av}{A vector; indexed elementwise.}
\item{fit}{Passed to \code{.smooth_at}.}
\item{n_folds}{See Usage.}
}
\description{
A step of the tmlcps_native implementation. Called by \code{effect_curve}. See the file header for the source the module follows.
}
\value{
The value of \code{best_h}, as built in the body.
}
\keyword{internal}
