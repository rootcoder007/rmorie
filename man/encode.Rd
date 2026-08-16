% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{encode}
\alias{encode}
\title{encode}
\usage{
encode(y_tilde, W, V_u, b, activation = "sigmoid")
}
\arguments{
\item{y_tilde}{See Usage.}
\item{W}{A vector; indexed elementwise.}
\item{V_u}{A vector; indexed elementwise.}
\item{b}{A vector; its length is taken and its elements indexed.}
\item{activation}{Passed to \code{.cdae_act}. Defaults to \code{"sigmoid"}.}
}
\description{
A step of the cdaeRC_native implementation. Called by \code{fit_cdae}, \code{recommend}. See the file header for the source the module follows.
}
\value{
The value of \code{z}, as built in the body.
}
\keyword{internal}
\examples{
encode(y_tilde = c(1, 2, 3, 4, 5, 6, 7, 8), W = c(1, 2, 3, 4, 5, 6, 7, 8), V_u = c(1, 2, 3, 4, 5, 6, 7, 8), b = 5L)
}
