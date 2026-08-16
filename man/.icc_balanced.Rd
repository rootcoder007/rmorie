% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.icc_balanced}
\alias{.icc_balanced}
\title{Long-format (value, group) into a subjects-by-raters matrix}
\usage{
.icc_balanced(y, group, who)
}
\arguments{
\item{y}{Passed to \code{.s03vec}.}
\item{group}{Passed to \code{.s03vec}.}
\item{who}{See Usage.}
}
\description{
A step of the icc1 implementation. Called by \code{Icc1}, \code{Icc2}, \code{Icc3}. See the file header for the source the module follows.
}
\value{
A list with \code{rows}, \code{n}, \code{k}.
}
\keyword{internal}
