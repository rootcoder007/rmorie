% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_burkov_term_frequency}
\alias{morie_burkov_term_frequency}
\title{Term frequency and TF-IDF (Burkov Ch 2)}
\usage{
morie_burkov_term_frequency(term, document, normalise = FALSE)
}
\arguments{
\item{term}{Query term.}
\item{document}{Token vector.}
\item{normalise}{Divide by document length.}
}
\description{
Term frequency and TF-IDF (Burkov Ch 2)
}
\value{
A list with \code{estimate}, \code{count}, \code{doc_length}, \code{normalised}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_burkov_term_frequency(V, V)
}
