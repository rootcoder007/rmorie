% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{LmsVarStep}
\alias{LmsVarStep}
\title{Eq. (3.204): eq. (3.203) with a step size that changes each sample}
\usage{
LmsVarStep(w, e, rvec, mu_n)
}
\arguments{
\item{w}{See Usage.}
\item{e}{See Usage.}
\item{rvec}{See Usage.}
\item{mu_n}{See Usage.}
}
\description{
Part of the rangayyan_adapt implementation; see the file header for the source it follows.
}
\value{
A list with \code{w_next}, \code{mu}, \code{e}, \code{order}, \code{time_varying}, \code{method}.
}
\keyword{internal}
