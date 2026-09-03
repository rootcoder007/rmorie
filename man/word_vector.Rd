% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{word_vector}
\alias{word_vector}
\title{word_vector}
\usage{
word_vector(word, Z, gram_index, n_min = 3, n_max = 6, boundary = TRUE, whole_word = TRUE, 
    hash_buckets = NULL) 
}
\arguments{
\item{word}{Passed to \code{subwords}.}
\item{Z}{A matrix; indexed by row and column.}
\item{gram_index}{Passed to \code{.gram_slot}.}
\item{n_min}{Passed to \code{subwords}. Defaults to \code{3}.}
\item{n_max}{Passed to \code{subwords}. Defaults to \code{6}.}
\item{boundary}{Passed to \code{subwords}. Defaults to \code{TRUE}.}
\item{whole_word}{Passed to \code{subwords}. Defaults to \code{TRUE}.}
\item{hash_buckets}{Passed to \code{.gram_slot}.}
}
\description{
A step of the fastxt_native implementation. Called by \code{fasttext}. See the file header for the source the module follows.
}
\value{
A list with \code{v}, \code{hit}.
}
\keyword{internal}
\examples{
word_vector(word = 5L, Z = c(1, 2, 3, 4, 5, 6, 7, 8),
  gram_index = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)))
}
