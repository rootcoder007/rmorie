% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tf_need}
\alias{.tf_need}
\title{.tf_need}
\usage{
.tf_need(x, name = "x", minlen = 2L)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{name}{Defaults to \code{"x"}.}
\item{minlen}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{2L}.}
}
\description{
A step of the rangayyan_tf implementation. Called by \code{AtomTfd}, \code{BiorDwt}, \code{CDemod} and 28 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{v}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.tf_need(V)
}
