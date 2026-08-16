% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_cf_rmst_pseudo}
\alias{.morie_cf_rmst_pseudo}
\title{Inverse-probability-of-censoring-weighted RMST pseudo outcome, whose}
\usage{
.morie_cf_rmst_pseudo(time, event, horizon)
}
\arguments{
\item{time}{A vector; its length is taken and its elements indexed.}
\item{event}{A vector; indexed elementwise.}
\item{horizon}{Numeric; combined arithmetically in the body.}
}
\description{
expectation is E[min(T, horizon)] under independent censoring.
}
\value{
The value of \code{ifelse}.
}
\keyword{internal}
\examples{
.morie_cf_rmst_pseudo(time = c(1, 2, 3, 4, 5, 6, 7, 8), event = c(0, 1, 0, 1, 1, 0, 1, 0), horizon = c(1, 2, 3, 4, 5, 6, 7, 8))
}
