% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{word_vector}
\alias{word_vector}
\title{word_vector}
\usage{
word_vector(word, Z, gram_index, n_min = 3, n_max = 6, boundary = TRUE, whole_word = TRUE, hash_buckets = NULL)
}
\arguments{
\item{word}{See Usage.}
\item{Z}{See Usage.}
\item{gram_index}{See Usage.}
\item{n_min}{Defaults to \code{3}.}
\item{n_max}{Defaults to \code{6}.}
\item{boundary}{Defaults to \code{TRUE}.}
\item{whole_word}{Defaults to \code{TRUE}.}
\item{hash_buckets}{Defaults to \code{NULL}.}
}
\description{
Part of the fastxt_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{v}, \code{hit}.
}
\keyword{internal}
