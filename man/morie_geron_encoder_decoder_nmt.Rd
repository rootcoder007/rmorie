% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_encoder_decoder_nmt}
\alias{morie_geron_encoder_decoder_nmt}
\title{Encoder-decoder seq2seq: teacher-forced loss and greedy decode (Geron Ch 14, hmnmt)}
\usage{
morie_geron_encoder_decoder_nmt(src, tgt, model, max_len = NULL, eos = NULL)
}
\arguments{
\item{src}{Source token sequence.}
\item{tgt}{Target token ids (reference).}
\item{model}{List with `encode(src)` and `decode(z, prefix)` functions.}
\item{max_len,eos}{Greedy decode cap and stop id.}
}
\description{
Encoder-decoder seq2seq: teacher-forced loss and greedy decode (Geron Ch 14, hmnmt)
}
\value{
A list with \code{loss}, \code{mean_loss}, \code{token_losses}, \code{perplexity}, \code{greedy}, \code{exact_match}, \code{z}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
model <- list(encode = function(s) length(s), decode = function(z, 
    prefix) c(0.2, 0.5, 0.3))
morie_geron_encoder_decoder_nmt(c(9, 9), c(1, 1), model)
}
