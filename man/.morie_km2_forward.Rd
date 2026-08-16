% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_km2_forward}
\alias{.morie_km2_forward}
\title{.morie_km2_forward}
\usage{
.morie_km2_forward(text, probs, maxlen)
}
\arguments{
\item{text}{A vector; its length is taken.}
\item{probs}{A vector; indexed elementwise.}
\item{maxlen}{Passed to \code{.morie_km2_pieces_by_end}.}
}
\description{
A step of the kamath_llm2_native implementation. Called by \code{morie_kamath_unigram_lm_tokenizer}, \code{morie_kamath_unigram_loglik}. See the file header for the source the module follows.
}
\value{
A list with \code{alpha}, \code{ends}.
}
\keyword{internal}
