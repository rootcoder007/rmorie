% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_c_tfidf}
\alias{morie_alammar_c_tfidf}
\title{morie_alammar_c_tfidf}
\usage{
morie_alammar_c_tfidf(term_counts_by_class, corpus_freq = NULL, A = NULL)
}
\arguments{
\item{term_counts_by_class}{Classes x terms count matrix.}
\item{corpus_freq,A}{Optional column sums and mean class size.}
}
\description{
morie_alammar_c_tfidf
}
\value{
A list with \code{weights}, \code{A}, \code{corpus_freq}, \code{top_term_per_class}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
