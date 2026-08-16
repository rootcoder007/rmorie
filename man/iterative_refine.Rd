% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{iterative_refine}
\alias{iterative_refine}
\title{iterative_refine}
\usage{
iterative_refine(alignment, scoring, tree = NULL, s_op = 2.4, max_iterate = 16L, seq_type = "aa", use_fft = TRUE, ...)
}
\arguments{
\item{alignment}{See Usage.}
\item{scoring}{See Usage.}
\item{tree}{Defaults to \code{NULL}.}
\item{s_op}{Defaults to \code{2.4}.}
\item{max_iterate}{Defaults to \code{16L}.}
\item{seq_type}{Defaults to \code{"aa"}.}
\item{use_fft}{Defaults to \code{TRUE}.}
\item{...}{Passed through.}
}
\description{
Part of the mafft_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{aln}, \code{score}, \code{rounds}.
}
\keyword{internal}
