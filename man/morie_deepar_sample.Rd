% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_deepar_sample}
\alias{morie_deepar_sample}
\title{Ancestral sampling: feed each draw back in}
\usage{
morie_deepar_sample(fit, z_history, horizon, n_samples = 200L, seed = 0L)
}
\arguments{
\item{fit}{A fit object from \code{morie_deepar_fit}.}
\item{z_history}{Numeric vector of history observations.}
\item{horizon}{Integer forecast horizon.}
\item{n_samples}{Integer number of trajectories.}
\item{seed}{Integer seed.}
}
\description{
Multi-step uncertainty has no closed form once the model consumes its own output, which is why the intervals widen with horizon without being told to.
}
\value{
A list of trajectories, each a length-\code{horizon} numeric   vector.
}
\keyword{internal}
