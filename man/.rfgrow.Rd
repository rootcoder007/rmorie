% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rfgrow}
\alias{.rfgrow}
\title{A node is list(var, thr, li, ri, value, n, drop).  A leaf has var =}
\usage{
.rfgrow(X, Y, rows, b, nodesize, mtry, q, env)
}
\arguments{
\item{X}{A matrix; passed to \code{ncol}.}
\item{Y}{A matrix; indexed by row and column.}
\item{rows}{A vector; its length is taken.}
\item{b}{Passed to \code{.rfcand}.}
\item{nodesize}{Numeric; combined arithmetically in the body.}
\item{mtry}{Passed to \code{.rfcand}.}
\item{q}{A count; the body uses it as \code{numeric(...)}.}
\item{env}{A list; the body reads \code{$k}, \code{$nodes}, \code{$s} from it.}
}
\description{
-1L Part of the mvsml_rf_shared implementation; see the file header for the source it follows.
}
\value{
The value of \code{idx}, as built in the body.
}
\keyword{internal}
