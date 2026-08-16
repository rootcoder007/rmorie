% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_km2_causal_lm_loss}
\alias{.morie_km2_causal_lm_loss}
\title{Kmclm\\'s core: causal-LM cross entropy over non-ignored positions}
\usage{
.morie_km2_causal_lm_loss(logits, targets, ignore_index = -100L)
}
\arguments{
\item{logits}{A matrix; indexed by row and column.}
\item{targets}{Coerced to integer by the body, with \code{as.integer}.}
\item{ignore_index}{Passed to \code{!=}. Defaults to \code{-100L}.}
}
\description{
A step of the kamath_llm2_native implementation. Called by \code{morie_kamath_instruction_tuning_loss}, \code{morie_kamath_llava_visual_instruction}. See the file header for the source the module follows.
}
\value{
A list with \code{loss}, \code{perplexity}, \code{token_losses}, \code{n_tokens}, \code{vocab_size}.
}
\keyword{internal}
\examples{
.morie_km2_causal_lm_loss(logits = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), targets = list(a = 1, b = 2))
}
