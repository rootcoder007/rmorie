% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.frns_favorable_rates}
\alias{.frns_favorable_rates}
\title{.frns_favorable_rates}
\usage{
.frns_favorable_rates(outcome, group, favorable)
}
\arguments{
\item{outcome}{A vector; indexed elementwise.}
\item{group}{See Usage.}
\item{favorable}{See Usage.}
}
\description{
A step of the frns_metrics implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{rates}, as built in the body.
}
\keyword{internal}
\examples{
.frns_favorable_rates(outcome = c(1, 2, 3, 4, 5, 6, 7, 8), group = c("a", "b", "c"), favorable = c(1, 2, 3, 4, 5, 6, 7, 8))
}
