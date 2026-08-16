% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Cohere}
\alias{Cohere}
\title{Eq (4.32).  The book is emphatic: computed from two single}
\usage{
Cohere(x, y, fs = 1, nperseg = NULL, noverlap = NULL)
}
\arguments{
\item{x}{See Usage.}
\item{y}{See Usage.}
\item{fs}{Defaults to \code{1}.}
\item{nperseg}{Defaults to \code{NULL}.}
\item{noverlap}{Defaults to \code{NULL}.}
}
\description{
observations the magnitude is UNITY AT EVERY FREQUENCY, which is incorrect; each density must be estimated by AVERAGING over several observations.  So a single segment is refused rather than returning the meaningless all-ones answer.
}
\value{
A list with \code{coherence}, \code{phase}, \code{sxx}, \code{syy}, \code{sxy}, \code{freqs}, \code{n_segments}, \code{nperseg}, \code{method}.
}
\keyword{internal}
