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
\item{image_embedding}{See Usage.}
\item{prompt_tokens}{See Usage.}
\item{decoder}{See Usage.}
\item{multimask}{Defaults to \code{TRUE}.}
}
\description{
A step of the samseg_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{masks}, \code{n_masks}, \code{multimask}, \code{method}, \code{note}.
}
\keyword{internal}
