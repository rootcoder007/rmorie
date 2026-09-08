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
\item{replicates}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{target_mean}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{target_se}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{tol}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.15}.}
}
\description{
A step of the tlboot_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{mean}, \code{se}, \code{mean_error}, \code{se_ratio}, \code{first_two_moments_ok}, \code{note}.
}
\keyword{internal}
\examples{
set.seed(3)
reps <- rnorm(500, 2, 0.3)
r <- moment_check(reps, target_mean = 2, target_se = 0.3)
str(r, max.level = 1)
}
