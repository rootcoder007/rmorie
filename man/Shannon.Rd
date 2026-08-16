% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Shannon}
\alias{Shannon}
\title{Eq (3.11): H = - sum p(eta_l) log2 p(eta_l), over L quantized values}
\usage{
Shannon(p, levels = NULL)
}
\arguments{
\item{p}{See Usage.}
\item{levels}{Defaults to \code{NULL}.}
}
\description{
Part of the rangayyan_ch3 implementation; see the file header for the source it follows.
}
\value{
A list with \code{entropy}, \code{units}, \code{levels}, \code{max_entropy}, \code{probabilities}, \code{method}.
}
\keyword{internal}
