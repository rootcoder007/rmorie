% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_blast}
\alias{morie_blast}
\title{morie_blast}
\usage{
morie_blast(query, subjects, w = 11L, match = 5, mismatch = -4, cutoff = NULL, X = 20, word_mode = "exact", threshold = NULL, matrix = NULL, alphabet = "ACGT", lam = NULL, K = NULL, max_hsps = NULL, letter_probs = NULL, pvalues = TRUE)
}
\arguments{
\item{query}{Passed to \code{morie_blstn}.}
\item{subjects}{Passed to \code{morie_blstn}.}
\item{w}{Passed to \code{morie_blstn}. Defaults to \code{11L}.}
\item{match}{Passed to \code{morie_blstn}. Defaults to \code{5}.}
\item{mismatch}{Passed to \code{morie_blstn}. Defaults to \code{-4}.}
\item{cutoff}{Passed to \code{morie_blstn}.}
\item{X}{Passed to \code{morie_blstn}. Defaults to \code{20}.}
\item{word_mode}{Passed to \code{morie_blstn}. Defaults to \code{"exact"}.}
\item{threshold}{Passed to \code{morie_blstn}.}
\item{matrix}{Passed to \code{morie_blstn}.}
\item{alphabet}{Passed to \code{morie_blstn}. Defaults to \code{"ACGT"}.}
\item{lam}{Passed to \code{morie_blstn}.}
\item{K}{Passed to \code{morie_blstn}.}
\item{max_hsps}{Passed to \code{morie_blstn}.}
\item{letter_probs}{Passed to \code{morie_blstn}.}
\item{pvalues}{Passed to \code{morie_blstn}. Defaults to \code{TRUE}.}
}
\description{
A step of the blstn_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{morie_blstn}.
}
\keyword{internal}
\examples{
morie_blast(query = 5L, subjects = c(1, 2, 3, 4, 5, 6, 7, 8))
}
