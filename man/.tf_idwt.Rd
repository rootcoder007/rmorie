% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tf_idwt}
\alias{.tf_idwt}
\title{.tf_idwt}
\usage{
.tf_idwt(a, details, lengths, wavelet)
}
\arguments{
\item{a}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{details}{A vector; its length is taken and its elements indexed.}
\item{lengths}{A vector; indexed elementwise.}
\item{wavelet}{Passed to \code{.tf_filters}.}
}
\description{
A step of the rangayyan_tf implementation. Called by \code{Mra}, \code{SwtDen}, \code{WtThresh}. See the file header for the source the module follows.
}
\value{
The value of \code{cur}, as built in the body.
}
\keyword{internal}
