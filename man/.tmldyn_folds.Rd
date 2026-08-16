% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tmldyn_folds}
\alias{.tmldyn_folds}
\title{.tmldyn_folds}
\usage{
.tmldyn_folds(n, n_folds)
}
\arguments{
\item{n}{A count; the body uses it as \code{seq_len(...)}.}
\item{n_folds}{See Usage.}
}
\description{
A step of the tmldyn_native implementation. Called by \code{morie_tmldyn}. See the file header for the source the module follows.
}
\value{
The value of \code{lapply}.
}
\keyword{internal}
