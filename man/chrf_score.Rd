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
\item{hypothesis}{Coerced to character by the body, with \code{as.character}.}
\item{reference}{A vector; its length is taken.}
\item{n_char}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{6L}.}
\item{beta}{Numeric; combined arithmetically in the body. Defaults to \code{2}.}
\item{remove_whitespace}{A flag; the body branches on it. Defaults to \code{TRUE}.}
\item{word_order}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{0L}.}
}
\description{
A step of the chrF_native implementation. Called by \code{morie_chrF}. See the file header for the source the module follows.
}
\value{
The value of \code{best}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
chrf_score(V, V)
}
