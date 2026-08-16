% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bnd_simplex}
\alias{.bnd_simplex}
\title{.bnd_simplex}
\usage{
.bnd_simplex(cv, A_ub = NULL, b_ub = NULL, A_eq = NULL, b_eq = NULL, lower, upper)
}
\arguments{
\item{cv}{See Usage.}
\item{A_ub}{Defaults to \code{NULL}.}
\item{b_ub}{Defaults to \code{NULL}.}
\item{A_eq}{Defaults to \code{NULL}.}
\item{b_eq}{Defaults to \code{NULL}.}
\item{lower}{See Usage.}
\item{upper}{See Usage.}
}
\description{
A small dense two-phase simplex with Bland\\\'s rule, written here because base R has no linear programming and this package does not take dependencies for what eighty lines can do. Solves min c\\\'x s.t. A_ub x <= b_ub, A_eq x = b_eq, l <= x <= u. Bland\\\'s rule (always the lowest-index eligible column and row) trades speed for a cycling-freedom guarantee, which is the right trade at the problem sizes identification bounds produce.
}
\value{
A list with \code{status}, \code{x}, \code{fun}.
}
\keyword{internal}
