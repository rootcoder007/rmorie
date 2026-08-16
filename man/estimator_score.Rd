% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{estimator_score}
\alias{estimator_score}
\title{estimator_score}
\usage{
estimator_score(hyp, src, ref, W, b = NULL)
}
\arguments{
\item{hyp}{See Usage.}
\item{src}{See Usage.}
\item{ref}{See Usage.}
\item{W}{A matrix; passed to \code{nrow}.}
\item{b}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the comet_native implementation. Called by \code{morie_comet}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{score}, \code{method}, \code{note}.
}
\keyword{internal}
