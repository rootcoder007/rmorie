% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.snmtst_split}
\alias{.snmtst_split}
\title{.snmtst_split}
\usage{
.snmtst_split(beta, n_pre, n_post)
}
\arguments{
\item{beta}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{n_pre}{A count; the body uses it as \code{seq_len(...)}.}
\item{n_post}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
A step of the snmtst_native implementation. Called by \code{identified_set}. See the file header for the source the module follows.
}
\value{
A list with \code{pre}, \code{post}.
}
\keyword{internal}
