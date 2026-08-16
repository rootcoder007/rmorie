% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_attend}
\alias{.morie_gr_attend}
\title{Attsdp / grsdpa kernel. mask: logical matrix, TRUE = attend}
\usage{
.morie_gr_attend(Q, K, V, mask = NULL)
}
\arguments{
\item{Q}{See Usage.}
\item{K}{See Usage.}
\item{V}{See Usage.}
\item{mask}{Defaults to \code{NULL}.}
}
\description{
Part of the geron_train_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{output}, \code{weights}, \code{scores}.
}
\keyword{internal}
