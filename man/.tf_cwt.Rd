% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tf_cwt}
\alias{.tf_cwt}
\title{.tf_cwt}
\usage{
.tf_cwt(x, scales, wavelet = "morlet", w0 = 5)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{scales}{A vector; its length is taken and its elements indexed.}
\item{wavelet}{Passed to \code{.tf_support}. Defaults to \code{"morlet"}.}
\item{w0}{Passed to \code{.tf_mother}. Defaults to \code{5}.}
}
\description{
A step of the rangayyan_tf implementation. Called by \code{CprWt}, \code{Cwt}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
