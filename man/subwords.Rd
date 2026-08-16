% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{subwords}
\alias{subwords}
\title{subwords}
\usage{
subwords(word, n_min = 3, n_max = 6, boundary = TRUE, whole_word = TRUE)
}
\arguments{
\item{word}{Coerced to character by the body, with \code{as.character}.}
\item{n_min}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{3}.}
\item{n_max}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{6}.}
\item{boundary}{A flag; the body branches on it. Defaults to \code{TRUE}.}
\item{whole_word}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the fastxt_native implementation. Called by \code{fasttext}, \code{word_vector}. See the file header for the source the module follows.
}
\value{
The value of \code{grams}, as built in the body.
}
\keyword{internal}
\examples{
subwords(word = 5L)
}
