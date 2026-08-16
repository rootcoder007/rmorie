% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.fit_q2}
\alias{.fit_q2}
\title{.fit_q2}
\usage{
.fit_q2(y, L0, A0, L1, A1, idx, ridge)
}
\arguments{
\item{y}{A vector; indexed elementwise.}
\item{L0}{A matrix; indexed by row and column.}
\item{A0}{A vector; indexed elementwise.}
\item{L1}{A matrix; indexed by row and column.}
\item{A1}{A vector; indexed elementwise.}
\item{idx}{Iterated over elementwise, with \code{lapply}.}
\item{ridge}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the tmldyn_native implementation. Called by \code{morie_tmldyn}, \code{rule_value_seq}, \code{sequential_blips}. See the file header for the source the module follows.
}
\value{
A list with \code{q2}, \code{b}.
}
\keyword{internal}
