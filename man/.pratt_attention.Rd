% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.pratt_attention}
\alias{.pratt_attention}
\title{.pratt_attention}
\usage{
.pratt_attention(H, W, b, u_context)
}
\arguments{
\item{H}{Passed to \code{.pratt_mat}.}
\item{W}{A matrix; passed to \code{t}.}
\item{b}{A vector; its length is taken.}
\item{u_context}{Passed to \code{.pratt_vec}.}
}
\description{
A step of the pratt_native implementation. Called by \code{.pratt_document_vector}, \code{.pratt_sentence_vector}. See the file header for the source the module follows.
}
\value{
A numeric value.
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
