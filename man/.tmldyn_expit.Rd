% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tmldyn_expit}
\alias{.tmldyn_expit}
\title{.tmldyn_expit}
\usage{
.tmldyn_expit(z)
}
\arguments{
\item{z}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the tmldyn_native implementation. Called by \code{.fluctuate}, \code{intervention_mechanism}, \code{morie_tmldyn}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
.tmldyn_expit(z = 5L)
}
