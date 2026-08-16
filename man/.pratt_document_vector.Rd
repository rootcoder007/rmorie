% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.pratt_document_vector}
\alias{.pratt_document_vector}
\title{.pratt_document_vector}
\usage{
.pratt_document_vector(H_sentences, W, b, u_s)
}
\arguments{
\item{H_sentences}{Passed to \code{.pratt_attention}.}
\item{W}{Passed to \code{.pratt_attention}.}
\item{b}{Passed to \code{.pratt_attention}.}
\item{u_s}{Passed to \code{.pratt_attention}.}
}
\description{
A step of the pratt_native implementation. Called by \code{morie_pratt}. See the file header for the source the module follows.
}
\value{
A list with \code{vector}, \code{alpha}.
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
\examples{
.pratt_document_vector(H_sentences = c(1, 2, 3, 4, 5, 6, 7, 8), W = c(1, 2, 3, 4, 5, 6, 7, 8), b = c(1, 2, 3, 4, 5, 6, 7, 8), u_s = c(1, 2, 3, 4, 5, 6, 7, 8))
}
