% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_glove}
\alias{morie_glove}
\title{Fit GloVe vectors}
\usage{
morie_glove(corpus, dim = 50, window = 10, epochs = 25, lr = 0.05, x_max = 100, alpha = 0.75, harmonic = TRUE, min_count = 1, seed = 0, combine = "sum")
}
\arguments{
\item{corpus}{See Usage.}
\item{dim}{Defaults to \code{50}.}
\item{window}{Defaults to \code{10}.}
\item{epochs}{Defaults to \code{25}.}
\item{lr}{Defaults to \code{0.05}.}
\item{x_max}{Defaults to \code{100}.}
\item{alpha}{Defaults to \code{0.75}.}
\item{harmonic}{Defaults to \code{TRUE}.}
\item{min_count}{Defaults to \code{1}.}
\item{seed}{Defaults to \code{0}.}
\item{combine}{Defaults to \code{"sum"}.}
}
\description{
Part of the glove_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{vectors}, \code{vocab}, \code{index}, \code{W}, \code{W_tilde}, \code{b}, \code{b_tilde}, \code{cooccurrence}, \code{loss_history}, \code{running_loss}, \code{final_loss}, \code{n_vocab}, \code{n_pairs}, \code{dim}, \code{window}, \code{harmonic}, \code{x_max}, \code{alpha}, \code{combine}, \code{method}.
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
