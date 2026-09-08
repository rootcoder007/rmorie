% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_muzero}
\alias{morie_muzero}
\title{Run MuZero\'s MCTS}
\usage{
morie_muzero(observation, actions, representation, dynamics, prediction, simulations = 50, 
    gamma = 0.997, c1 = 1.25, c2 = 19652, dirichlet_alpha = NULL, exploration_fraction = 0.25, 
    temperature = 1, seed = 0) 
}
\arguments{
\item{observation}{Passed to representation.}
\item{actions}{Action space.}
\item{representation,dynamics,prediction}{h, g, f callables.}
\item{simulations}{Number of simulations.}
\item{gamma}{Discount.}
\item{c1,c2}{pUCT constants.}
\item{dirichlet_alpha}{Optional root Dirichlet noise.}
\item{exploration_fraction}{Noise weight at the root.}
\item{temperature}{Temperature on visit-count distribution.}
\item{seed}{Seed for root noise.}
}
\description{
Run MuZero\'s MCTS
}
\value{
A list with policy, action, value, visits, Q, prior, and   bookkeeping.
}
\keyword{internal}
