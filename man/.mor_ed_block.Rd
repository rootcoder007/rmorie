% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_ed_block}
\alias{.mor_ed_block}
\title{Sum of D over the 0-based half-open block [a1, b1) x [a2, b2)}
\usage{
.mor_ed_block(P, a1, b1, a2, b2)
}
\arguments{
\item{P}{A matrix; indexed by row and column.}
\item{a1}{Numeric; combined arithmetically in the body.}
\item{b1}{Numeric; combined arithmetically in the body.}
\item{a2}{Numeric; combined arithmetically in the body.}
\item{b2}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the e_div_native implementation. Called by \code{.mor_ed_qhat}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
