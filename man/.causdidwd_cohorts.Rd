% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.causdidwd_cohorts}
\alias{.causdidwd_cohorts}
\title{.causdidwd_cohorts}
\usage{
.causdidwd_cohorts(first_treated, period)
}
\arguments{
\item{first_treated}{A vector; its length is taken and its elements indexed.}
\item{period}{See Usage.}
}
\description{
A step of the causdidwd_native implementation. Called by \code{morie_etwfe}, \code{morie_imputation}. See the file header for the source the module follows.
}
\value{
A list with \code{G}, \code{ts}, \code{order}.
}
\keyword{internal}
