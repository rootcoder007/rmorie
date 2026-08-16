% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.flxipt_folds}
\alias{.flxipt_folds}
\title{Disjoint validation blocks -------------------------------------}
\usage{
.flxipt_folds(n, V)
}
\arguments{
\item{n}{A count; the body uses it as \code{seq_len(...)}.}
\item{V}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the flxipt_native implementation. Called by \code{super_learner}. See the file header for the source the module follows.
}
\value{
The value of \code{lapply}.
}
\keyword{internal}
