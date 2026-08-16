% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_prioritized_replay}
\alias{morie_geron_prioritized_replay}
\title{Prioritized experience replay: TD-error priority sampling, IS weights (Geron Ch 19, hmper)}
\usage{
morie_geron_prioritized_replay(buffer, alpha = 0.6, beta = 0.4, eps = 1e-06, batch_size = NULL, seed = 0)
}
\arguments{
\item{buffer}{TD errors, or a list of transitions with a `td_error` field.}
\item{alpha,beta,eps}{Prioritisation, IS and floor controls.}
\item{batch_size,seed}{Optional draw.}
}
\description{
Prioritized experience replay: TD-error priority sampling, IS weights (Geron Ch 19, hmper)
}
\value{
A list with \code{priorities}, \code{probabilities}, \code{weights}, \code{indices}, \code{alpha}, \code{beta}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
