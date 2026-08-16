% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gram_slot}
\alias{.gram_slot}
\title{.gram_slot}
\usage{
.gram_slot(g, gram_index, hash_buckets)
}
\arguments{
\item{g}{Passed to \code{.fnv1a}.}
\item{gram_index}{A vector; indexed elementwise.}
\item{hash_buckets}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the fastxt_native implementation. Called by \code{fasttext}, \code{word_vector}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
