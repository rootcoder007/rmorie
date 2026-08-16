% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{selective_ssm_step}
\alias{selective_ssm_step}
\title{selective_ssm_step}
\usage{
selective_ssm_step(x, h, A, B, C, delta, rule = "zoh")
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{h}{A vector; its length is taken and its elements indexed.}
\item{A}{A vector; its length is taken.}
\item{B}{See Usage.}
\item{C}{A vector; its length is taken.}
\item{delta}{See Usage.}
\item{rule}{Defaults to \code{"zoh"}.}
}
\description{
A step of the mambss_native implementation. Called by \code{selective_scan}. See the file header for the source the module follows.
}
\value{
A list with \code{h}, \code{y}.
}
\keyword{internal}
