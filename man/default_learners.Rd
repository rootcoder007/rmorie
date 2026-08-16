% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{default_learners}
\alias{default_learners}
\title{Default library, in the shape Pirracchio et al. use ------------}
\usage{
default_learners(p, ridge_penalties = c(0, 1, 10))
}
\arguments{
\item{p}{Passed to \code{>=}.}
\item{ridge_penalties}{Defaults to \code{c(0, 1, 10)}.}
}
\description{
A step of the flxipt_native implementation. Called by \code{super_learner}. See the file header for the source the module follows.
}
\value{
The value of \code{lib}, as built in the body.
}
\keyword{internal}
\examples{
default_learners(p = 0.5)
}
