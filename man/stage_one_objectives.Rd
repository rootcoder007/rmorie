% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{stage_one_objectives}
\alias{stage_one_objectives}
\title{stage_one_objectives}
\usage{
stage_one_objectives(query_out, text_out, temperature = 0.07)
}
\arguments{
\item{query_out}{See Usage.}
\item{text_out}{See Usage.}
\item{temperature}{Defaults to \code{0.07}.}
}
\description{
Part of the blip2v_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{per_query_similarity}, \code{image_text_similarity}, \code{best_query}, \code{logit}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
stage_one_objectives(V, V)
}
