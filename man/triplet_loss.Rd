% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{triplet_loss}
\alias{triplet_loss}
\title{triplet_loss}
\usage{
triplet_loss(better, worse, src, ref, margin = 1)
}
\arguments{
\item{better}{See Usage.}
\item{worse}{See Usage.}
\item{src}{See Usage.}
\item{ref}{See Usage.}
\item{margin}{Defaults to \code{1}.}
}
\description{
Part of the comet_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{loss}, \code{source_term}, \code{reference_term}, \code{satisfied}, \code{note}.
}
\keyword{internal}
