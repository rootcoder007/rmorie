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
\examples{
img <- matrix(0:15, 4, 4, byrow = TRUE)
v <- morie_alammar_vit_patch_embedding(img, 2, diag(4))
y <- c(0, 0, 0, 1, 1, 1)
morie_alammar_chain_prompting("3", list(function(y, x) paste("double", 
    x), function(y, x) paste("add one to", y)), function(p) {
    v <- as.integer(sub(".* ", "", p))
    if (grepl("double", p)) 
        as.character(v * 2)
    else as.character(v + 1)
})
}
