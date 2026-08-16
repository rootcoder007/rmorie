% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_kamath_ragas_faithfulness}
\alias{morie_kamath_ragas_faithfulness}
\title{RAGAS faithfulness (Kamath Ch 7)}
\usage{
morie_kamath_ragas_faithfulness(answer, context, entails = NULL)
}
\arguments{
\item{answer}{Claims.}
\item{context}{Retrieved passage(s).}
\item{entails}{Optional judge.}
}
\description{
faithfulness = supported claims / total claims. `answer` is a character vector of claims or one string split on sentence punctuation; `entails` an optional judge function(claim, context) -> logical, defaulting to the same strict lexical containment the Python uses (every content token of the claim present in the context) -- under-reports rather than hallucinating support, and the judge used is named in the payload. Mirrors morie.fn.kmfait.
}
\value{
A list with \code{estimate}, \code{supported}, \code{n_claims}, \code{claims}, \code{claim_supported}, \code{judge}, \code{n}, \code{method}.
}
\keyword{internal}
