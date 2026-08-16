% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_svdd_solve_dual}
\alias{.morie_svdd_solve_dual}
\title{.morie_svdd_solve_dual}
\usage{
.morie_svdd_solve_dual(K, C, n, tol, max_iter)
}
\arguments{
\item{K}{A matrix; indexed by row and column.}
\item{C}{Numeric; combined arithmetically in the body.}
\item{n}{A count; the body uses it as \code{seq_len(...)}.}
\item{tol}{See Usage.}
\item{max_iter}{See Usage.}
}
\description{
A step of the svdd_native implementation. Called by \code{morie_svdd}. See the file header for the source the module follows.
}
\value{
The value of \code{alpha}, as built in the body.
}
\keyword{internal}
