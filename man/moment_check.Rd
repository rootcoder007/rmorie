% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{moment_check}
\alias{moment_check}
\title{moment_check}
\usage{
moment_check(replicates, target_mean, target_se, tol = 0.15)
}
\arguments{
\item{replicates}{See Usage.}
\item{target_mean}{See Usage.}
\item{target_se}{See Usage.}
\item{tol}{Defaults to \code{0.15}.}
}
\description{
Part of the tlboot_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{mean}, \code{se}, \code{mean_error}, \code{se_ratio}, \code{first_two_moments_ok}, \code{note}.
}
\keyword{internal}
