% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{amortised_cost}
\alias{amortised_cost}
\title{amortised_cost}
\usage{
amortised_cost(encoder_ms, decoder_ms, n_prompts)
}
\arguments{
\item{encoder_ms}{See Usage.}
\item{decoder_ms}{See Usage.}
\item{n_prompts}{See Usage.}
}
\description{
Part of the samseg_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{total_ms}, \code{per_prompt_ms}, \code{naive_ms}, \code{speedup}, \code{interactive}, \code{note}.
}
\keyword{internal}
