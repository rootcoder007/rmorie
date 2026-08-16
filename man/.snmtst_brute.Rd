% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.snmtst_brute}
\alias{.snmtst_brute}
\title{.snmtst_brute}
\usage{
.snmtst_brute(point, c, M, grid, post = NULL, lin = NULL, lv = NULL)
}
\arguments{
\item{point}{Numeric; combined arithmetically in the body.}
\item{c}{A vector; its length is taken.}
\item{M}{Numeric; combined arithmetically in the body.}
\item{grid}{A count; the body uses it as \code{seq_len(...)}.}
\item{post}{Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.}
\item{lin}{Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.}
\item{lv}{Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.}
}
\description{
A step of the snmtst_native implementation. Called by \code{identified_set}. See the file header for the source the module follows.
}
\value{
A list with \code{lo}, \code{hi}.
}
\keyword{internal}
