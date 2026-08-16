% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rsosort}
\alias{.rsosort}
\title{Indices that sort v ascending, ties broken by the lower index.}
\usage{
.rsosort(v)
}
\arguments{
\item{v}{See Usage.}
}
\description{
Written out rather than delegated to order() because the C-step selects "the h smallest distances": which of two tied points is taken changes the subset.
}
\value{
The value of \code{idx}, as built in the body.
}
\keyword{internal}
