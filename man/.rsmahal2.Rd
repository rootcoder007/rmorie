% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rsmahal2}
\alias{.rsmahal2}
\title{Squared Mahalanobis distances of every row of X; NULL if S is}
\usage{
.rsmahal2(X, mu, S)
}
\arguments{
\item{X}{A matrix; indexed by row and column.}
\item{mu}{A vector; its length is taken.}
\item{S}{Passed to \code{.rslufactor}.}
}
\description{
singular Part of the helpers_rouss implementation; see the file header for the source it follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
