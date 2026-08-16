% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_km2_causal_lm_loss}
\alias{.morie_km2_causal_lm_loss}
\title{Kmclm\\\'s core: causal-LM cross entropy over non-ignored positions}
\usage{
.morie_km2_causal_lm_loss(logits, targets, ignore_index = -100L)
}
\arguments{
\item{logits}{See Usage.}
\item{targets}{See Usage.}
\item{ignore_index}{Defaults to \code{-100L}.}
}
\description{
Part of the kamath_llm2_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{loss}, \code{perplexity}, \code{token_losses}, \code{n_tokens}, \code{vocab_size}.
}
\keyword{internal}
