% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_word_hits}
\alias{morie_word_hits}
\title{morie_word_hits}
\usage{
morie_word_hits(query, subject, w, mode = "exact", threshold = NULL, match = 5, mismatch = -4, matrix = NULL, alphabet = "ACGT")
}
\arguments{
\item{query}{Coerced to character by the body, with \code{as.character}.}
\item{subject}{Coerced to character by the body, with \code{as.character}.}
\item{w}{A count; the body uses it as \code{seq_len(...)}.}
\item{mode}{One of \code{"exact"}, \code{"neighborhood"}. Defaults to \code{"exact"}.}
\item{threshold}{The body requires: blstn: mode='neighborhood' needs a threshold T.}
\item{match}{Defaults to \code{5}.}
\item{mismatch}{Defaults to \code{-4}.}
\item{matrix}{Optional; may be \code{NULL}. A matrix; indexed by row and column.}
\item{alphabet}{Character; passed to \code{strsplit}. Defaults to \code{"ACGT"}.}
}
\description{
A step of the blstn_native implementation. Called by \code{morie_blstn}. See the file header for the source the module follows.
}
\value{
The value of \code{hits}, as built in the body.
}
\keyword{internal}
