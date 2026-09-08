% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{t5enc_format_regression}
\alias{t5enc_format_regression}
\title{t5enc_format_regression}
\usage{
t5enc_format_regression(value, increment = 0.2, lo = 1, hi = 5) 
}
\arguments{
\item{value}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{increment}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.2}.}
\item{lo}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
\item{hi}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{5}.}
}
\description{
A step of the t5enc_native implementation. Called by \code{morie_t5enc}. See the file header for the source the module follows.
}
\value{
A character value.
}
\references{
----------
Raffel, C., Shazeer, N., Roberts, A., Lee, K., Narang, S., Matena, M.,
Zhou, Y., Li, W. & Liu, P. J. (2020) "Exploring the Limits of Transfer
Learning with a Unified Text-to-Text Transformer", *Journal of Machine
Learning Research* 21(140), 1-67, arXiv:1910.10683. The text-to-text
framework in which every task is fed text and produces text, with task
prefixes; the treatment of classification by emitting the label text
and of regression by emitting a rounded number as a string, with
outputs that do not match any label counted as wrong; the span
corruption pre-training objective with sentinels and the ablations
selecting 15\% corruption and mean span length 3; and the simplified
relative position embeddings shared across layers, bucketed by
relative offset with logarithmically growing buckets.

Vaswani, A., Shazeer, N., Parmar, N., Uszkoreit, J., Jones, L.,
Gomez, A. N., Kaiser, L. & Polosukhin, I. (2017) "Attention Is All You
Need", *NIPS 2017*, 5998-6008, arXiv:1706.03762. The encoder-decoder
being modified.

Devlin, J., Chang, M.-W., Lee, K. & Toutanova, K. (2019) "BERT:
Pre-training of Deep Bidirectional Transformers for Language
Understanding", *NAACL-HLT 2019*, 4171-4186,
doi:10.18653/v1/N19-1423. The single-token masking objective span
corruption replaces.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
t5enc_format_regression(V)
}
