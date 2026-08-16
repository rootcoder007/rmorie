% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_score_distribution}
\alias{morie_score_distribution}
\title{morie_score_distribution}
\usage{
morie_score_distribution(match = 5, mismatch = -4, letter_probs = NULL, matrix = NULL, subject_probs = NULL)
}
\arguments{
\item{match}{Defaults to \code{5}.}
\item{mismatch}{Defaults to \code{-4}.}
\item{letter_probs}{Defaults to \code{NULL}.}
\item{matrix}{Optional; may be \code{NULL}. A vector; indexed elementwise.}
\item{subject_probs}{Defaults to \code{NULL}.}
}
\description{
A step of the blstn_native implementation. Called by \code{morie_karlin_altschul}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
morie_score_distribution()
}
