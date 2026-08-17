% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tagRC}
\alias{morie_tagRC}
\title{morie_tagRC}
\usage{
morie_tagRC(triples, focus, d = 0.7, weight = 0.9, iters = 200)
}
\arguments{
\item{triples}{Passed to \code{folkrank}.}
\item{focus}{Passed to \code{folkrank}.}
\item{d}{Passed to \code{folkrank}. Defaults to \code{0.7}.}
\item{weight}{Passed to \code{folkrank}. Defaults to \code{0.9}.}
\item{iters}{Passed to \code{folkrank}. Defaults to \code{200}.}
}
\description{
A step of the tagRC_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{folkrank}.
}
\keyword{internal}
\examples{
triples <- list(c("u1", "t1", "r1"), c("u1", "t2", "r2"),
                c("u2", "t1", "r1"), c("u2", "t3", "r3"))
morie_tagRC(triples, focus = "t:t1")
}
