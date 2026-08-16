% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Pf}
\alias{Pf}
\title{Pf}
\usage{
Pf(q, df1, df2, lower_tail = TRUE)
}
\arguments{
\item{q}{Iterated over elementwise, with \code{vapply}.}
\item{df1}{Numeric; combined arithmetically in the body.}
\item{df2}{Numeric; combined arithmetically in the body.}
\item{lower_tail}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the dist_native implementation. Called by \code{Qf}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
Pf(4.26, 3, 10)
}
