% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ch_rv}
\alias{.ch_rv}
\title{RV_q: the partial R2 an omitted confounder needs, eq. (9)}
\usage{
.ch_rv(t, df, q = 1)
}
\arguments{
\item{t}{Numeric; passed to \code{abs}.}
\item{df}{Numeric; passed to \code{sqrt}.}
\item{q}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
}
\description{
A step of the chzlt implementation. Called by \code{Chzlt}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
