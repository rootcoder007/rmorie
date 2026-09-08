% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_chrF}
\alias{morie_chrF}
\title{morie_chrF}
\usage{
morie_chrF(hypothesis, reference, n_char = 6L, beta = 2, remove_whitespace = TRUE, 
    word_order = 0L) 
}
\arguments{
\item{hypothesis}{Passed to \code{chrf_score}.}
\item{reference}{Passed to \code{chrf_score}.}
\item{n_char}{Passed to \code{chrf_score}. Defaults to \code{6L}.}
\item{beta}{Passed to \code{chrf_score}. Defaults to \code{2}.}
\item{remove_whitespace}{Passed to \code{chrf_score}. Defaults to \code{TRUE}.}
\item{word_order}{Passed to \code{chrf_score}. Defaults to \code{0L}.}
}
\description{
A step of the chrF_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{chrf_score}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_chrF(V, V)
}
