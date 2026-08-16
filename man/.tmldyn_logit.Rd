% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tmldyn_logit}
\alias{.tmldyn_logit}
\title{.tmldyn_logit}
\usage{
.tmldyn_logit(p)
}
\arguments{
\item{p}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the tmldyn_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.tmldyn_logit(V)
}
