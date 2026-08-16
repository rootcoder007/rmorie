% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{chrf_score}
\alias{chrf_score}
\title{chrf_score}
\usage{
chrf_score(hypothesis, reference, n_char = 6L, beta = 2, remove_whitespace = TRUE, word_order = 0L)
}
\arguments{
\item{hypothesis}{See Usage.}
\item{reference}{See Usage.}
\item{n_char}{Defaults to \code{6L}.}
\item{beta}{Defaults to \code{2}.}
\item{remove_whitespace}{Defaults to \code{TRUE}.}
\item{word_order}{Defaults to \code{0L}.}
}
\description{
Part of the chrF_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{best}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
chrf_score(V, V)
}
