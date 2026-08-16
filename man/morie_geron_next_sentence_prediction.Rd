% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_next_sentence_prediction}
\alias{morie_geron_next_sentence_prediction}
\title{Next sentence prediction: BERT-style input assembly plus a linear \[CLS\] head (Geron Ch 15, hmnsp)}
\usage{
morie_geron_next_sentence_prediction(sent_A, sent_B, encoder = NULL, w = NULL, b = 0, label = NULL)
}
\arguments{
\item{sent_A,sent_B}{Tokenised sentences.}
\item{encoder}{Optional `encoder(tokens, segments) -> h`.}
\item{w,b}{Head weights/bias.}
\item{label}{Optional 0/1 label for the loss.}
}
\description{
Next sentence prediction: BERT-style input assembly plus a linear \[CLS\] head (Geron Ch 15, hmnsp)
}
\value{
A list with \code{tokens}, \code{segment_ids}, \code{cls_vector}, \code{logit}, \code{probability}, \code{prediction}, \code{loss}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_geron_next_sentence_prediction(V, V)
}
