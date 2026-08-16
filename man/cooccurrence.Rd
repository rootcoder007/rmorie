% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{cooccurrence}
\alias{cooccurrence}
\title{Word-word co-occurrence counts over a symmetric context window}
\usage{
cooccurrence(corpus, window = 10, harmonic = TRUE, min_count = 1)
}
\arguments{
\item{corpus}{See Usage.}
\item{window}{Defaults to \code{10}.}
\item{harmonic}{Defaults to \code{TRUE}.}
\item{min_count}{Defaults to \code{1}.}
}
\description{
Sec. 4.2: a token d positions away contributes 1/d to the count when harmonic = TRUE.
}
\value{
A list with \code{X}, \code{vocab}, \code{index}.
}
\references{
----------
Pennington, J., Socher, R. & Manning, C. D. (2014) "GloVe: Global
Vectors for Word Representation", Proceedings of EMNLP 2014,
1532-1543, doi:10.3115/v1/D14-1162.  Equations (8) and (9), Sec. 4.2
for the harmonic weighting and the AdaGrad training.

Duchi, J., Hazan, E. & Singer, Y. (2011) "Adaptive subgradient methods
for online learning and stochastic optimization", JMLR 12, 2121-2159
-- the AdaGrad the paper uses.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
cooccurrence(V)
}
