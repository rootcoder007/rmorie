% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.dueldqn_argmax}
\alias{.dueldqn_argmax}
\title{.dueldqn_argmax}
\usage{
.dueldqn_argmax(x)
}
\arguments{
\item{x}{A vector; its length is taken and its elements indexed.}
}
\description{
Double-DQN target: the ONLINE net picks the action, the TARGET net values it. Using one net for both over-estimates because the max of a noisy estimate is biased upward and the same noise supplies the value. ---------------------------------------------------------------------------
}
\value{
The value of \code{b}, as built in the body.
}
\keyword{internal}
