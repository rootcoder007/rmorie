% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{k02pchi}
\alias{k02pchi}
\title{k02pchi}
\usage{
k02pchi(q, df)
}
\arguments{
\item{q}{See Usage.}
\item{df}{See Usage.}
}
\description{
A step of the k02util implementation. Called by \code{mafix}, \code{mai2}, \code{marndm} and 1 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{stats::pchisq}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
k02pchi(V, V)
}
