% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_pratt}
\alias{morie_pratt}
\title{morie_pratt}
\usage{
morie_pratt(word_states, Ww, bw, u_w, Ws, bs, u_s, Wc, bc) 
}
\arguments{
\item{word_states}{See Usage.}
\item{Ww}{Passed to \code{.pratt_sentence_vector}.}
\item{bw}{Passed to \code{.pratt_sentence_vector}.}
\item{u_w}{Passed to \code{.pratt_sentence_vector}.}
\item{Ws}{Passed to \code{.pratt_document_vector}.}
\item{bs}{Passed to \code{.pratt_document_vector}.}
\item{u_s}{Passed to \code{.pratt_document_vector}.}
\item{Wc}{A matrix; passed to \code{\%*\%}.}
\item{bc}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the pratt_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{probabilities}, \code{document_vector}, \code{sentence_attention}, \code{word_attention}, \code{n_sentences}, \code{method}, \code{note}.
}
\references{
----------
Yang, Z., Yang, D., Dyer, C., He, X., Smola, A. & Hovy, E. (2016)
"Hierarchical Attention Networks for Document Classification",
NAACL-HLT 2016, 1480-1489, doi:10.18653/v1/N16-1174.

Bahdanau, D., Cho, K. & Bengio, Y. (2015) "Neural Machine Translation
by Jointly Learning to Align and Translate", ICLR 2015,
arXiv:1409.0473.

Sukhbaatar, S., Szlam, A., Weston, J. & Fergus, R. (2015)
"End-To-End Memory Networks", NIPS 2015, 2440-2448, arXiv:1503.08895.
}
\keyword{internal}
