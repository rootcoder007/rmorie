% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_chain_prompting}
\alias{morie_alammar_chain_prompting}
\title{Prompt chains, multi-query retrieval, the ReAct loop}
\usage{
morie_alammar_chain_prompting(x, prompts, model)
}
\arguments{
\item{x}{Original input.}
\item{prompts}{List of   function(previous_output, original_input) -> prompt.}
\item{model}{function(prompt) -> text.}
}
\description{
Prompt chains, multi-query retrieval, the ReAct loop
}
\value{
A list with \code{final_output}, \code{steps}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
