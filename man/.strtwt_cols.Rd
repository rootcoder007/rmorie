% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.strtwt_cols}
\alias{.strtwt_cols}
\title{.strtwt_cols}
\usage{
.strtwt_cols(X, n, nm)
}
\arguments{
\item{X}{Optional; may be \code{NULL}. Passed to \code{.s03mat}.}
\item{n}{A count; the body uses it as \code{matrix(...)}.}
\item{nm}{Passed to \code{sprintf}.}
}
\description{
A step of the Strtwt implementation. Called by \code{Strtwt}. See the file header for the source the module follows.
}
\value{
The value of \code{M}, as built in the body.
}
\keyword{internal}
