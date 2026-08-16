% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_km2_backward}
\alias{.morie_km2_backward}
\title{.morie_km2_backward}
\usage{
.morie_km2_backward(text, probs, maxlen)
}
\arguments{
\item{text}{A vector; its length is taken.}
\item{probs}{A vector; indexed elementwise.}
\item{maxlen}{Numeric; passed to \code{min}.}
}
\description{
A step of the kamath_llm2_native implementation. Called by \code{morie_kamath_unigram_lm_tokenizer}. See the file header for the source the module follows.
}
\value{
The value of \code{beta}, as built in the body.
}
\keyword{internal}
