% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_blstn}
\alias{morie_blstn}
\title{morie_blstn}
\usage{
morie_blstn(query, subjects, w = 11L, match = 5, mismatch = -4, cutoff = NULL, X = 20, word_mode = "exact", threshold = NULL, matrix = NULL, alphabet = "ACGT", lam = NULL, K = NULL, max_hsps = NULL, letter_probs = NULL, pvalues = TRUE)
}
\arguments{
\item{query}{See Usage.}
\item{subjects}{See Usage.}
\item{w}{Numeric; combined arithmetically in the body. Defaults to \code{11L}.}
\item{match}{Passed to \code{morie_word_hits}. Defaults to \code{5}.}
\item{mismatch}{Passed to \code{morie_word_hits}. Defaults to \code{-4}.}
\item{cutoff}{Defaults to \code{NULL}.}
\item{X}{Defaults to \code{20}.}
\item{word_mode}{Passed to \code{morie_word_hits}. Defaults to \code{"exact"}.}
\item{threshold}{Passed to \code{morie_word_hits}.}
\item{matrix}{Optional; may be \code{NULL}. A matrix; indexed by row and column.}
\item{alphabet}{A vector; its length is taken. Defaults to \code{"ACGT"}.}
\item{lam}{Optional; may be \code{NULL}. Passed to \code{morie_blast_pvalue}.}
\item{K}{Optional; may be \code{NULL}. Passed to \code{morie_blast_pvalue}.}
\item{max_hsps}{Defaults to \code{NULL}.}
\item{letter_probs}{Defaults to \code{NULL}.}
\item{pvalues}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the blstn_native implementation. Called by \code{morie_blast}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{hsps}, \code{best_score}, \code{n_hsps}, \code{n_hits}, \code{w}, \code{cutoff}, \code{X}, \code{word_mode}, \code{lam}, \code{K}, \code{karlin_altschul}, \code{note}, \code{method}.
}
\keyword{internal}
\examples{
morie_blstn(query = 5L, subjects = c(1, 2, 3, 4, 5, 6, 7, 8))
}
