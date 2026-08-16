% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{LinDSep}
\alias{LinDSep}
\title{Section 10.4.2 with a fitted cut.  The midpoint of the projected}
\usage{
LinDSep(X, y)
}
\arguments{
\item{X}{See Usage.}
\item{y}{See Usage.}
}
\description{
means is optimal only for equal priors AND equal variances, so the error-minimizing cut is the default and the midpoint is reported beside it.  Both errors are resubstitution errors -- measured on the data that chose the cut -- so they are optimistic; Section 10.10.3 is the book\'s warning, and KFoldCv or LooCv gives an honest figure.
}
\value{
A list with \code{w}, \code{threshold}, \code{midpoint_threshold}, \code{classes}, \code{first_class_is_above}, \code{training_errors}, \code{midpoint_errors}, \code{training_accuracy}, \code{n}, \code{projected}, \code{midpoint_optimal_only_for_equal_priors_and_spread}, \code{resubstitution_error_is_optimistic}, \code{method}.
}
\keyword{internal}
\examples{
XC <- matrix(c(1, 1, 1.4, 0.8, 0.7, 1.3, 1.1, 1.6, 3, 3, 3.3, 
    2.7, 2.6, 3.2, 3.1, 3.5), ncol = 2, byrow = TRUE)
YC <- c(0, 0, 0, 0, 1, 1, 1, 1)
LinDSep(XC, YC)
}
