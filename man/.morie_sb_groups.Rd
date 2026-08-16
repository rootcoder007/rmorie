% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_sb_groups}
\alias{.morie_sb_groups}
\title{.morie_sb_groups}
\usage{
.morie_sb_groups(h, bins, cutoff, exact)
}
\arguments{
\item{h}{A vector; indexed elementwise.}
\item{bins}{Optional; may be \code{NULL}. A vector; its length is taken.}
\item{cutoff}{See Usage.}
\item{exact}{A flag; the body branches on it.}
}
\description{
A step of the schaben_native implementation. Called by \code{morie_cressie_hawkins}, \code{morie_matheron_estimator}. See the file header for the source the module follows.
}
\value{
A list with \code{centres}, \code{idx}.
}
\keyword{internal}
