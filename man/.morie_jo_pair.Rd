% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_jo_pair}
\alias{.morie_jo_pair}
\title{.morie_jo_pair}
\usage{
.morie_jo_pair(y, yhat)
}
\arguments{
\item{y}{Passed to \code{.morie_jo_vec}.}
\item{yhat}{Passed to \code{.morie_jo_vec}.}
}
\description{
A step of the ts_joseph implementation. Called by \code{morie_mapets}, \code{morie_pinball}, \code{morie_relmae} and 4 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{a}, \code{b}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_jo_pair(V, V)
}
