% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_jo_resblock}
\alias{.morie_jo_resblock}
\title{.morie_jo_resblock}
\usage{
.morie_jo_resblock(x, w1, b1, w2, b2, wskip)
}
\arguments{
\item{x}{Passed to \code{.morie_jo_matvec}.}
\item{w1}{Passed to \code{.morie_jo_matvec}.}
\item{b1}{Numeric; combined arithmetically in the body.}
\item{w2}{Passed to \code{.morie_jo_matvec}.}
\item{b2}{Numeric; combined arithmetically in the body.}
\item{wskip}{Passed to \code{.morie_jo_matvec}.}
}
\description{
A step of the ts_joseph implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{.morie_jo_ln}.
}
\keyword{internal}
