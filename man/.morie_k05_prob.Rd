% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_k05_prob}
\alias{.morie_k05_prob}
\title{Branch on the sign so exp never overflows for large |z|}
\usage{
.morie_k05_prob(theta, a, b, c, u, D = 1)
}
\arguments{
\item{theta}{Numeric; combined arithmetically in the body.}
\item{a}{Numeric; combined arithmetically in the body.}
\item{b}{Numeric; combined arithmetically in the body.}
\item{c}{Numeric; combined arithmetically in the body.}
\item{u}{Numeric; combined arithmetically in the body.}
\item{D}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
}
\description{
A step of the k05_tranche2 implementation. Called by \code{.morie_k05_info}, \code{morie_tcc}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
