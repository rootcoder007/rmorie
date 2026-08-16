% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rfboot}
\alias{.rfboot}
\title{.rfboot}
\usage{
.rfboot(b, n)
}
\arguments{
\item{b}{Numeric; combined arithmetically in the body.}
\item{n}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
A step of the mvsml_rf_shared implementation. Called by \code{.rfforest}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
