% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rmrl_eps_greedy}
\alias{.rmrl_eps_greedy}
\title{Epsilon-greedy with ties broken UNIFORMLY AT RANDOM. The table}
\usage{
.rmrl_eps_greedy(row, A, epsilon, rng)
}
\arguments{
\item{row}{See Usage.}
\item{A}{See Usage.}
\item{epsilon}{See Usage.}
\item{rng}{See Usage.}
}
\description{
starts all-zero, so every action ties; deterministic tie-breaking would turn the initial behaviour into a systematic drift. rng is a .ghc_rng state: the Python arm draws rng.random() from np.random.default_rng(seed), bit-identical to .ghc_unif here.
}
\value{
The value of \code{[[}.
}
\keyword{internal}
