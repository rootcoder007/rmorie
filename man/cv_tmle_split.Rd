% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{cv_tmle_split}
\alias{cv_tmle_split}
\title{cv_tmle_split}
\usage{
cv_tmle_split(n, V = 10L, seed = 0L)
}
\arguments{
\item{n}{A count; the body uses it as \code{seq_len(...)}.}
\item{V}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{10L}.}
\item{seed}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0L}.}
}
\description{
A step of the tlhaltm_native implementation. Called by \code{morie_tlhaltm}. See the file header for the source the module follows.
}
\value{
A list with \code{folds}, \code{training}, \code{V}, \code{note}.
}
\keyword{internal}
