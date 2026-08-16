% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{progressive_align}
\alias{progressive_align}
\title{progressive_align}
\usage{
progressive_align(seqs, scoring, tree = NULL, seq_type = "aa", s_op = 2.4, use_fft = TRUE, ...)
}
\arguments{
\item{seqs}{A vector; its length is taken and its elements indexed.}
\item{scoring}{See Usage.}
\item{tree}{Defaults to \code{NULL}.}
\item{seq_type}{Defaults to \code{"aa"}.}
\item{s_op}{Defaults to \code{2.4}.}
\item{use_fft}{A flag; the body branches on it. Defaults to \code{TRUE}.}
\item{...}{Passed through.}
}
\description{
A step of the mafft_native implementation. Called by \code{mafft_alignment}, \code{morie_mafft}. See the file header for the source the module follows.
}
\value{
A vector, from \code{unlist}.
}
\keyword{internal}
