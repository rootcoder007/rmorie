% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{project_to_llm}
\alias{project_to_llm}
\title{project_to_llm}
\usage{
project_to_llm(query_out, W, b = NULL)
}
\arguments{
\item{query_out}{See Usage.}
\item{W}{See Usage.}
\item{b}{Defaults to \code{NULL}.}
}
\description{
Part of the blip2v_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{soft_prompt}, \code{n_tokens}, \code{dim}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
project_to_llm(V, V)
}
