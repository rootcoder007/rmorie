% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{promptable_segment}
\alias{promptable_segment}
\title{promptable_segment}
\usage{
promptable_segment(image_embedding, prompt_tokens, decoder, multimask = TRUE) 
}
\arguments{
\item{image_embedding}{Passed to \code{decoder}.}
\item{prompt_tokens}{Passed to \code{decoder}.}
\item{decoder}{The body requires: samseg: the decoder returned no mask; the task requires a valid mask for ANY prompt.}
\item{multimask}{Coerced to logical by the body, with \code{as.logical}. Defaults to \code{TRUE}.}
}
\description{
A step of the samseg_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{masks}, \code{n_masks}, \code{multimask}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
decoder <- function(emb, tokens, multi) list(matrix(runif(16), 4, 4),
                                             matrix(runif(16), 4, 4))
r <- promptable_segment(image_embedding = matrix(rnorm(64), 8, 8),
                        prompt_tokens = matrix(rnorm(16), 2, 8), decoder)
is.list(r)
}
