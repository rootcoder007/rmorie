% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sequence_probabilities}
\alias{morie_sequence_probabilities}
\title{morie_sequence_probabilities}
\usage{
morie_sequence_probabilities(beta, gamma, x, alpha, y0, link = "logit")
}
\arguments{
\item{beta}{See Usage.}
\item{gamma}{See Usage.}
\item{x}{A matrix; passed to \code{as.matrix}.}
\item{alpha}{See Usage.}
\item{y0}{See Usage.}
\item{link}{One of \code{"logit"}, \code{"probit"}. Defaults to \code{"logit"}.}
}
\description{
A step of the bnshrt_native implementation. Called by \code{morie_in_identified_set}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
