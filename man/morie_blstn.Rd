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
\item{w}{Defaults to \code{11L}.}
\item{match}{Defaults to \code{5}.}
\item{mismatch}{Defaults to \code{-4}.}
\item{cutoff}{Defaults to \code{NULL}.}
\item{X}{Defaults to \code{20}.}
\item{word_mode}{Defaults to \code{"exact"}.}
\item{threshold}{Defaults to \code{NULL}.}
\item{matrix}{Defaults to \code{NULL}.}
\item{alphabet}{Defaults to \code{"ACGT"}.}
\item{lam}{Defaults to \code{NULL}.}
\item{K}{Defaults to \code{NULL}.}
\item{max_hsps}{Defaults to \code{NULL}.}
\item{letter_probs}{Defaults to \code{NULL}.}
\item{pvalues}{Defaults to \code{TRUE}.}
}
\description{
Part of the blstn_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{hsps}, \code{best_score}, \code{n_hsps}, \code{n_hits}, \code{w}, \code{cutoff}, \code{X}, \code{word_mode}, \code{lam}, \code{K}, \code{karlin_altschul}, \code{note}, \code{method}.
}
\keyword{internal}
\examples{
morie_blstn(query = 5L, subjects = c(1, 2, 3, 4, 5, 6, 7, 8))
}
