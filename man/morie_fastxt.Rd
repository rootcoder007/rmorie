% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_fastxt}
\alias{morie_fastxt}
\title{morie_fastxt}
\usage{
morie_fastxt(corpus, dim = 50, n_min = 3, n_max = 6, window = 5, epochs = 5, lr = 0.05, negative = 5, min_count = 1, boundary = TRUE, whole_word = TRUE, hash_buckets = NULL, seed = 0)
}
\arguments{
\item{corpus}{See Usage.}
\item{dim}{Defaults to \code{50}.}
\item{n_min}{Defaults to \code{3}.}
\item{n_max}{Defaults to \code{6}.}
\item{window}{Defaults to \code{5}.}
\item{epochs}{Defaults to \code{5}.}
\item{lr}{Defaults to \code{0.05}.}
\item{negative}{Defaults to \code{5}.}
\item{min_count}{Defaults to \code{1}.}
\item{boundary}{Defaults to \code{TRUE}.}
\item{whole_word}{Defaults to \code{TRUE}.}
\item{hash_buckets}{Defaults to \code{NULL}.}
\item{seed}{Defaults to \code{0}.}
}
\description{
A step of the fastxt_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{fasttext}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_fastxt(V)
}
