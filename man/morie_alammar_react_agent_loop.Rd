% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_react_agent_loop}
\alias{morie_alammar_react_agent_loop}
\title{morie_alammar_react_agent_loop}
\usage{
morie_alammar_react_agent_loop(query, tools, model, max_steps = 5)
}
\arguments{
\item{tools}{Named list of functions.}
\item{max_steps}{Budget.}
}
\description{
morie_alammar_react_agent_loop
}
\value{
A list with \code{answer}, \code{trace}, \code{steps_used}, \code{exhausted}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
calc <- function(x) as.character(eval(parse(text = x)))
model <- function(ctx) {
  if (length(ctx) == 1L) {
    list(thought = "need arithmetic", tool = "calc", input = "6 * 7")
  } else {
    list(final = "42")
  }
}
r <- morie_alammar_react_agent_loop("what is 6 times 7?",
                                    list(calc = calc), model)
str(r, max.level = 1)
}
